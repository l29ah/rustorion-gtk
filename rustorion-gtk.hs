{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DuplicateRecordFields, RecordWildCards #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.Coerce
import Data.Default
import Data.Function ((&))
import Data.IORef
import qualified Data.Map as M
import Data.Map ((!), (!?))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk as GTK
import SDL.Init
import SDL.Mixer
import System.Directory
import System.Exit

import CertGen
import RPC
import Types

toColor :: Types.Color -> GTK.Color
toColor (Types.Color r g b) = GTK.Color (round $ r * 65535) (round $ g * 65535) (round $ b * 65535)

makeScrollable scroll = do
	hadj <- scrolledWindowGetHAdjustment scroll
	vadj <- scrolledWindowGetVAdjustment scroll
	mousePressCoords <- newIORef (0, 0)

	on scroll motionNotifyEvent $ tryEvent $ do
		[Button3] <- eventModifierMouse	-- FIXME figure out why it receives all the events regardless of widgetGetEvents output
		(x, y) <- eventCoordinates
		(diffx, diffy) <- liftIO $ readIORef mousePressCoords
		liftIO $ adjustmentSetValue hadj $ diffx - x
		liftIO $ adjustmentSetValue vadj $ diffy - y
		eventRequestMotions

	on scroll buttonPressEvent $ tryEvent $ do
		RightButton <- eventButton
		adjx <- liftIO $ adjustmentGetValue hadj
		adjy <- liftIO $ adjustmentGetValue vadj
		(mx, my) <- eventCoordinates
		liftIO $ writeIORef mousePressCoords (adjx + mx, adjy + my)

scaleFactor = 20
scaleCoord x = 100 + (round $ x / scaleFactor)

makeShipWidget :: Types.Color -> IO DrawingArea
makeShipWidget Types.Color {..} = do
	widget <- drawingAreaNew
	set widget [ widgetCanFocus := True ]
	it <- iconThemeGetDefault
	-- TODO gtk_icon_theme_lookup_by_gicon_for_scale
	let size = 20
	(Just shipPix) <- iconThemeLoadIcon it ("ship" :: String) size IconLookupGenericFallback
	(Just reticlePix) <- iconThemeLoadIcon it ("reticle" :: String) size IconLookupGenericFallback
	widgetSetSizeRequest widget size size
	on widget draw $ do
		setSourceRGB (realToFrac r) (realToFrac g) (realToFrac b)
		paint
		setOperator OperatorDestAtop
		setSourcePixbuf shipPix 0 0
		paint
		drawReticle <- liftIO $ widgetGetIsFocus widget
		when drawReticle $ do
			setOperator OperatorOver
			setSourcePixbuf reticlePix 0 0
			paint
	on widget focus $ const $ widgetQueueDraw widget >> pure True
	widgetAddEvents widget [FocusChangeMask]
	pure widget

addShip :: TVar UIState -> Fixed -> UniverseView -> (Ship -> IO ()) -> Ship -> IO ()
addShip uiState layout view onClick ship@Ship { uuid = shid } = do
	UIState {..} <- readTVarIO uiState
	let empireColor = color $ (empires view) ! ((to $ ships_in_empires view) ! shid)
	butt <- makeShipWidget empireColor
	set butt [ widgetOpacity := 0.9 ]
	after butt buttonPressEvent $ tryEvent $ do
		LeftButton <- eventButton
		liftIO $ do
			writeIORef selectedObject $ Just $ coerce shid
			widgetGrabFocus butt
			onClick ship
	let ssid = (to $ ships_in_star_systems view) ! shid
	let (UniverseLocation x y) = location $ (star_systems view) ! ssid
	let shipButtonYOffset = 25
	fixedPut layout butt ((scaleCoord $ x - fst galaxyDisplayOffsets), (shipButtonYOffset + (scaleCoord $ y - snd galaxyDisplayOffsets)))

makeStarSystemWidget :: Types.Color -> IO DrawingArea
makeStarSystemWidget Types.Color {..} = do
	widget <- drawingAreaNew
	set widget [ widgetCanFocus := True ]
	it <- iconThemeGetDefault
	-- TODO gtk_icon_theme_lookup_by_gicon_for_scale
	let size = 48
	(Just starPix) <- iconThemeLoadIcon it ("star" :: String) size IconLookupGenericFallback
	(Just reticlePix) <- iconThemeLoadIcon it ("reticle" :: String) size IconLookupGenericFallback
	widgetSetSizeRequest widget size size
	on widget draw $ do
		-- set the star system border color to the empire color and make it transparent
		setSourceRGB (realToFrac r) (realToFrac g) (realToFrac b)
		let lineWidth = 4
		setLineWidth lineWidth
		arc ((fromIntegral size) / 2) ((fromIntegral size) / 2) ((fromIntegral size) / 2 - lineWidth / 2) 0 (2 * pi)
		stroke
		setSourcePixbuf starPix 0 0
		paint
		drawReticle <- liftIO $ widgetGetIsFocus widget
		when drawReticle $ do
			setSourcePixbuf reticlePix 0 0
			paint
	on widget focus $ const $ widgetQueueDraw widget >> pure True
	widgetAddEvents widget [FocusChangeMask]
	pure widget

addStarSystem :: IORef (Maybe (ID Void)) -> Fixed -> (StarSystem -> IO ()) -> (Double, Double) -> (Maybe Empire, StarSystem) -> IO ()
addStarSystem selectedObject layout onClick (xoff, yoff) (empire, ss@StarSystem {..}) = do
	butt <- makeStarSystemWidget $ maybe (Types.Color 0.5 0.5 0.5) color empire
	set butt [ widgetOpacity := 0.9 ]

	after butt buttonPressEvent $ tryEvent $ do
		LeftButton <- eventButton
		liftIO $ do
			writeIORef selectedObject $ Just $ coerce uuid
			widgetGrabFocus butt
			onClick ss
	let (UniverseLocation x y) = location
	-- place the buttons in the layout so they can be realized
	fixedPut layout butt ((scaleCoord $ x - xoff), (scaleCoord $ y - yoff))
	after butt realize $ do
		-- center the star system buttons on their locations
		(Rectangle x y w h) <- widgetGetAllocation butt
		fixedMove layout butt (x - (w `div` 2), y - (h `div` 2))
	pure ()

addStarSystems uiState layout onClick systems = do
	let offsets = (minimum $ map (ulx . location . snd) systems, minimum $ map (uly . location . snd) systems)
	atomically $ modifyTVar uiState $ \s -> s { galaxyDisplayOffsets = offsets }
	st <- readTVarIO uiState
	mapM_ (addStarSystem (selectedObject st) layout onClick offsets) systems

drawShipMoveOrder (xoff, yoff) (UniverseLocation x1 y1) (UniverseLocation x2 y2) = do
	setLineWidth 2.5
	setSourceRGB 0 1 0
	let sourceX = fromIntegral $ scaleCoord $ x1 - xoff
	let sourceY = fromIntegral $ scaleCoord $ y1 - yoff
	let destX = fromIntegral $ scaleCoord $ x2 - xoff
	let destY = fromIntegral $ scaleCoord $ y2 - yoff
	moveTo sourceX sourceY
	lineTo destX destY
	let arrowHeadLen = 15
	let arrowHeadAngle = pi / 6
	let lineAngle = atan $ (destY - sourceY) / (destX - sourceX)
	let absAngle = lineAngle + arrowHeadAngle
	let angle1 = if signum (destX - sourceX) > 0 then absAngle else absAngle - pi
	lineTo (destX - arrowHeadLen * cos angle1) (destY - arrowHeadLen * sin angle1)
	moveTo destX destY
	let angle2 = angle1 - 2 * arrowHeadAngle
	lineTo (destX - arrowHeadLen * cos angle2) (destY - arrowHeadLen * sin angle2)
	stroke

drawLane (xoff, yoff) (UniverseLocation x1 y1) (UniverseLocation x2 y2) = do
	setLineWidth 1.5
	setSourceRGB 0.6 0.6 0.6
	moveTo (fromIntegral $ scaleCoord $ x1 - xoff) (fromIntegral $ scaleCoord $ y1 - yoff)
	lineTo (fromIntegral $ scaleCoord $ x2 - xoff) (fromIntegral $ scaleCoord $ y2 - yoff)
	stroke

drawLanes offsets UniverseView {..} = do
	-- black interstellar space
	setSourceRGB 0 0 0
	paint

	mapM_ (\(id1, ids) -> mapM_ (\id2 ->
				drawLane offsets (location $ star_systems ! id1) (location $ star_systems ! id2)
			) ids
		) $ M.toList starlanes

drawSystemIdentifiers crownPix (xoff, yoff) UniverseView {..} annotatedStarSystems = do
	setSourceRGB 1 1 1
	let systemNameYOffset = 35
	let crownYOffset = 18
	mapM_ (\(empire, StarSystem {..}) -> do
			let UniverseLocation x y = location
			setFontSize 16
			exts <- textExtents name
			moveTo ((fromIntegral $ scaleCoord $ x - xoff) - textExtentsWidth exts / 2) ((fromIntegral $ scaleCoord $ y - yoff) - systemNameYOffset)
			showText name
			when (maybe False (\e -> capital e == Just uuid) empire) $ do
				save
				setSourcePixbuf crownPix (fromIntegral $ scaleCoord $ x - xoff) ((fromIntegral $ scaleCoord $ y - yoff) - systemNameYOffset - crownYOffset)
				crownPat <- getSource
				let Types.Color {..} = color $ fromJust empire
				setSourceRGB (realToFrac r) (realToFrac g) (realToFrac b)
				mask crownPat
				restore
		) $ M.elems annotatedStarSystems

annotateStarSystems UniverseView {..} = M.fromList $ map (\(id, ss) ->
		let empire = fmap (empires !) ((to star_systems_in_empires) !? id) in
		(id, (empire, ss))
	) $ M.toList $ star_systems

turnWaiter host key cert = do
	-- first we get the current turn data and utilize it
	let port = 4433
	conn <- rpcConnect host port key cert
	windowRef <- newIORef =<< postGUISync windowNew
	handleNewTurn conn windowRef

	-- then we're waiting for the next turns and handle them
	let backconnect_port = 4434
	backconn <- rpcConnect host backconnect_port key cert
	forever $ do
		rpcHandle backconn
		-- assume it's a turn change
		forkIO $ load "resources/newturn.ogg" >>= play
		handleNewTurn conn windowRef

makeWindow = do
	w <- windowNew
	set w [windowTitle := ("rustorion-gtk" :: String)]

	-- or better https://askubuntu.com/questions/153549/how-to-detect-a-computers-physical-screen-size-in-gtk
	scr <- fmap fromJust screenGetDefault
	x <- screenGetWidth scr
	y <- screenGetHeight scr
	windowSetDefaultSize w x y
	windowFullscreen w
	on w deleteEvent $ liftIO $ exitWith ExitSuccess
	pure w

handleNewTurn conn windowRef = do
	view <- getView conn
	print view
	so <- newIORef Nothing
	usw <- newIORef Nothing
	uiState <- newTVarIO $ UIState
		{ galaxyDisplayOffsets = (0, 0)
		, selectedObject = so
		, pendingShipActions = mempty
		, pendingStarSystemActions = mempty
		, updateShipWindow = usw
		}
	postGUISync $ do
		-- purge the old window
		readIORef windowRef >>= widgetDestroy
		w <- makeWindow
		writeIORef windowRef w

		topPaned <- vPanedNew
		containerAdd w topPaned

		topPanel <- hBoxNew False 0
		panedPack1 topPaned topPanel False False

		turnLabel <- labelNew $ Just ("Turn " ++ (show $ turn_number view))
		boxPackStart topPanel turnLabel PackNatural 0

		readyButton <- checkButtonNew
		readyButtonLabel <- labelNew $ Just ("Ready" :: String)
		containerAdd readyButton readyButtonLabel
		boxPackStart topPanel readyButton PackNatural 0
		on readyButton buttonActivated $ do
			gotReady <- toggleButtonGetActive readyButton
			readyToAct <- widgetGetSensitive readyButton
			when (gotReady && readyToAct) $ do
				readyButton & widgetSetSensitive $ False
				labelSetText readyButtonLabel ("sending..." :: String)
				readyButton & toggleButtonSetActive $ False
				toggleButtonSetInconsistent readyButton True
				setActions conn []
				-- after successful action submission
				toggleButtonSetInconsistent readyButton False
				labelSetText readyButtonLabel ("Ready" :: String)
				readyButton & toggleButtonSetActive $ True
				readyButton & widgetSetSensitive $ True

		panels <- vPanedNew
		panedPack2 topPaned panels True True

		infoLabel <- labelNew (Nothing :: Maybe String)
		labelSetLineWrap infoLabel True
		panedPack2 panels infoLabel False False

		universeScroll <- scrolledWindowNew Nothing Nothing
		scrolledWindowSetPolicy universeScroll PolicyAutomatic PolicyAutomatic
		makeScrollable universeScroll
		panedPack1 panels universeScroll True True

		-- put interactive items over the galaxy map background
		overlay <- overlayNew
		scrolledWindowAddWithViewport universeScroll overlay

		starlaneLayer <- drawingAreaNew
		containerAdd overlay starlaneLayer

		layout <- fixedNew
		overlayAdd overlay layout	-- put it over the starlane map

		-- make overlay respect the size of the layout
		sg <- sizeGroupNew SizeGroupBoth
		sizeGroupAddWidget sg starlaneLayer
		sizeGroupAddWidget sg layout

		-- cache the information about star system ownership
		let annotatedStarSystems = annotateStarSystems view

		-- draw our view content
		addStarSystems uiState layout (labelSetText infoLabel . show) $ M.elems $ annotatedStarSystems
		mapM_ (addShip uiState layout view (\s@Ship {..} -> do
				let setShipInfo label Ship {..} = do
					let shipEmpire = (to $ ships_in_empires view) ! uuid
					let shipLocation = (to $ ships_in_star_systems view) ! uuid
					let shipsAtLocation = (from $ ships_in_star_systems view) ! shipLocation
					let numberOfShipsInStack = length $ filter (\Ship {..} -> shipEmpire == (to $ ships_in_empires view) ! uuid) $ map (\shid -> (ships view) ! shid) shipsAtLocation
					let showShipInfo = T.concat [T.pack $ show numberOfShipsInStack, " ships owned by ", (\Empire {..} -> name) $ (empires view) ! shipEmpire]
					labelSetText label showShipInfo
				uis <- readTVarIO uiState
				setShipInfo infoLabel s
			)) $ M.elems $ ships view
		UIState { galaxyDisplayOffsets = offsets } <- readTVarIO uiState
		it <- iconThemeGetDefault
		(Just crownPix) <- iconThemeLoadIcon it ("crown" :: String) 16 IconLookupGenericFallback
		on starlaneLayer draw $ do
			drawLanes offsets view
			drawSystemIdentifiers crownPix offsets view annotatedStarSystems

		widgetShowAll w

main = do
	-- getArgs
	[key, cert] <- unsafeInitGUIForThreadedRTS
	missingAuthData <- fmap (not . and) $ sequence $ map doesFileExist [key, cert]
	when missingAuthData $ makeKeyCert key cert

	SDL.Init.initialize [InitAudio]
	-- 22050Hz by default, seriously!?
	let audio = def { audioFrequency = max 44100 $ audioFrequency def }
	openAudio audio 16384

	-- request a dark theme variant
	sets <- fmap fromJust $ settingsGetDefault
	settingsSetLongProperty sets ("gtk-application-prefer-dark-theme" :: String) 1 []
	-- permit putting images on buttons
	settingsSetLongProperty sets ("gtk-button-images" :: String) 1 []

	-- tell gtk where to look for icons
	iconTheme <- iconThemeGetDefault
	iconThemePrependSearchPath iconTheme "resources"

	let host = "localhost"
	forkIO $ turnWaiter host key cert
	mainGUI
