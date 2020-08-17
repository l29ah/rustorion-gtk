{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DuplicateRecordFields, RecordWildCards #-}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (assert)
import Control.Monad
import Control.Monad.Loops
import Data.Default
import Data.Function ((&))
import qualified Data.Function as F
import Data.IORef
import Data.List
import qualified Data.Map as M
import Data.Map ((!), (!?))
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk as GTK
import SDL.Init
import SDL.Mixer
import System.Directory
import System.Exit

import Actions
import CertGen
import RPC
import RPC.Translate
import Types as My

toColor :: My.Color -> GTK.Color
toColor (My.Color r g b) = GTK.Color (round $ r * 65535) (round $ g * 65535) (round $ b * 65535)

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

makeShipWidget :: My.Color -> IO DrawingArea
makeShipWidget My.Color {..} = do
	widget <- drawingAreaNew
	set widget [ widgetCanFocus := True ]
	it <- iconThemeGetDefault
	-- TODO gtk_icon_theme_lookup_by_gicon_for_scale
	let size = 20
	(Just shipPix) <- iconThemeLoadIcon it ("ship" :: Text) size IconLookupGenericFallback
	(Just reticlePix) <- iconThemeLoadIcon it ("reticle" :: Text) size IconLookupGenericFallback
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

-- |adds icons of fleets present in a given star system
addFleets :: TVar UIState -> Fixed -> (Fleet -> IO ()) -> [Fleet] -> IO ()
addFleets uiState layout onClick fleets = do
	let fleetN = length fleets
	let fleetAngles = map (\x -> (fromIntegral x) * 2 * pi / (fromIntegral fleetN)) [0..]
	let fleetIconDistance = 25
	let fleetIconOffset angle = (round $ fleetIconDistance * cos angle, round $ fleetIconDistance * sin angle)
	sequence_ $ zipWith (\angle fleet -> addFleet (fleetIconOffset angle) uiState layout onClick fleet) fleetAngles fleets

addFleet :: (Int, Int) -> TVar UIState -> Fixed -> (Fleet -> IO ()) -> Fleet -> IO ()
addFleet (xoffset, yoffset) uiState layout onClick fleet@Fleet {..} = do
	UIState {..} <- readTVarIO uiState
	butt <- makeShipWidget $ maybe def empireColor $ fleetOwner
	set butt [ widgetOpacity := 0.9 ]
	after butt buttonPressEvent $ tryEvent $ do
		LeftButton <- eventButton
		liftIO $ do
			writeIORef selectedObject $ Just fleet
			widgetGrabFocus butt
			onClick fleet
	let (UniverseLocation x y) = starSystemLocation fleetLocation
	fixedPut layout butt (xoffset + (scaleCoord $ x - fst galaxyDisplayOffsets), (yoffset + (scaleCoord $ y - snd galaxyDisplayOffsets)))

makeStarSystemWidget :: My.Color -> IO DrawingArea
makeStarSystemWidget My.Color {..} = do
	widget <- drawingAreaNew
	set widget [ widgetCanFocus := True ]
	it <- iconThemeGetDefault
	-- TODO gtk_icon_theme_lookup_by_gicon_for_scale
	let size = 48
	(Just starPix) <- iconThemeLoadIcon it ("star" :: Text) size IconLookupGenericFallback
	(Just reticlePix) <- iconThemeLoadIcon it ("reticle" :: Text) size IconLookupGenericFallback
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

addStarSystem :: UniverseView -> IO () -> (([My.Action] -> [My.Action]) -> IO ()) -> IORef (Maybe Fleet) -> Fixed -> (StarSystem -> IO ()) -> (Double, Double) -> StarSystem -> IO ()
addStarSystem view redraw adjustActions selectedObject layout onClick (xoff, yoff) ss@StarSystem {..} = do
	butt <- makeStarSystemWidget $ maybe (My.Color 0.5 0.5 0.5) empireColor $ starSystemOwner
	set butt [ widgetOpacity := 0.9 ]

	after butt buttonPressEvent $ tryEvent $ do
		buttonClicked <- eventButton
		liftIO $ case buttonClicked of
			LeftButton -> do
				writeIORef selectedObject Nothing
				widgetGrabFocus butt
				onClick ss
			RightButton -> do
				maybeSelected <- readIORef selectedObject
				maybe (pure ()) (\selected -> do
						let canJump = isAdjacent $ fleetLocation selected
						when (fleetOwner selected == controlledEmpire view && canJump) $ do
							-- order a ship to move there
							adjustActions $ moveFleet selected starSystemID
							-- redraw the starlane map
							redraw
					) maybeSelected
			_ -> pure ()

	let (UniverseLocation x y) = starSystemLocation

	-- place the buttons in the layout so they can be realized
	widgetShowAll butt
	let layoutCoords@(layoutX, layoutY) = ((scaleCoord $ x - xoff), (scaleCoord $ y - yoff))
	fixedPut layout butt layoutCoords

	-- wait for the widgets to get evaluated
	whileM_ (fmap (> 0) eventsPending) $ mainIterationDo False
	-- so we can know their desired size
	Requisition w h <- widgetSizeRequest butt
	-- to place their centers at the desired coordinates
	assert (w > 0 && h > 0) $ fixedMove layout butt (layoutX - (w `div` 2), layoutY - (h `div` 2))

	-- add a capture button when we can capture a system with our fleet in it
	when starSystemCanCapture $ do
		captureButt <- checkButtonNew
		set captureButt [ widgetOpacity := 0.9 ]
		captureButtLabel <- labelNew (Nothing :: Maybe Text)
		labelSetMarkup captureButtLabel ("<span foreground=\"red\">capture</span>" :: Text)
		containerAdd captureButt captureButtLabel
		-- put it under the star system button
		fixedPut layout captureButt (layoutX, layoutY + (h `div` 2))
		widgetShowAll captureButt
		void $ on captureButt toggled $ do
			activated <- toggleButtonGetActive captureButt
			case activated of
				True -> adjustActions $ captureStarSystem starSystemID $ empireID $ fromJust $ controlledEmpire view
				False -> adjustActions $ dontCaptureStarSystem starSystemID

addStarSystems view adjustActions uiState layout onClick systems = do
	let offsets = (minimum $ map (ulx . starSystemLocation) systems, minimum $ map (uly . starSystemLocation) systems)
	atomically $ modifyTVar' uiState $ \s -> s { galaxyDisplayOffsets = offsets }
	st <- readTVarIO uiState
	mapM_ (addStarSystem view (redrawStarlaneLayer st) adjustActions (selectedObject st) layout onClick offsets) systems

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

drawShipMoveOrders offsets (UniverseView {..}) pendingActions = do
	actions <- liftIO $ readTVarIO pendingActions
	let shipMoveActions = filter (\act -> case act of; MoveShip _ _ -> True; _ -> False) actions
	mapM_ (\(MoveShip id toid) -> do
			let ship = mapShips ! id
			let toLocation = starSystemLocation $ mapStarSystems ! toid
			drawShipMoveOrder offsets (starSystemLocation $ fromJust $ shipLocation ship) toLocation
		) shipMoveActions

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

	mapM_ (\ss1 -> mapM_ (\ss2 ->
				drawLane offsets (starSystemLocation ss1) (starSystemLocation ss2)
			) $ starSystemLanes ss1
		) $ starSystems

drawSystemIdentifiers crownPix (xoff, yoff) UniverseView {..} = do
	setSourceRGB 1 1 1
	let systemNameYOffset = 35
	let crownYOffset = 18
	mapM_ (\ss -> do
			let UniverseLocation x y = starSystemLocation ss
			setFontSize 16
			exts <- textExtents $ starSystemName ss
			moveTo ((fromIntegral $ scaleCoord $ x - xoff) - textExtentsWidth exts / 2) ((fromIntegral $ scaleCoord $ y - yoff) - systemNameYOffset)
			showText $ starSystemName ss
			when ((starSystemOwner ss >>= empireCapital) == Just ss) $ do
				save
				setSourcePixbuf crownPix (fromIntegral $ scaleCoord $ x - xoff) ((fromIntegral $ scaleCoord $ y - yoff) - systemNameYOffset - crownYOffset)
				crownPat <- getSource
				let My.Color {..} = empireColor $ fromJust $ starSystemOwner ss
				setSourceRGB (realToFrac r) (realToFrac g) (realToFrac b)
				mask crownPat
				restore
		) starSystems

annotateUniverseView :: UniverseView -> AnnotatedUniverseView
annotateUniverseView v@UniverseView {..} = AnnotatedUniverseView
	{ view = v
	, fleets = makePseudoFleets v
	}

-- |assumes the input is grouped by empires and star systems
groupedShipsToFleet :: [Ship] -> Fleet
groupedShipsToFleet ships = Fleet
	{ fleetShips = ships
	, fleetLocation = fromJust $ shipLocation $ head ships
	, fleetOwner = shipOwner $ head ships
	}

makePseudoFleets :: UniverseView -> [Fleet]
makePseudoFleets UniverseView {..} = map groupedShipsToFleet $ concat $ map (groupSortBy (fmap starSystemID . shipLocation)) $ groupSortBy (fmap empireID . shipOwner) ships
	where	groupSortBy byWhat = groupBy ((==) `F.on` byWhat) . sortBy (compare `F.on` byWhat)

turnWaiter host key cert = do
	-- first we get the current turn data and utilize it
	let port = 4433
	conn <- rpcConnect host port key cert
	window <- postGUISync makeWindow
	handleNewTurn conn window

	-- then we're waiting for the next turns and handle them
	let backconnect_port = 4434
	backconn <- rpcConnect host backconnect_port key cert
	forever $ do
		rpcHandle backconn
		-- assume it's a turn change
		forkIO $ load "resources/newturn.ogg" >>= play
		handleNewTurn conn window

makeWindow = do
	w <- windowNew
	set w [windowTitle := ("rustorion-gtk" :: Text)]

	-- or better https://askubuntu.com/questions/153549/how-to-detect-a-computers-physical-screen-size-in-gtk
	scr <- fmap fromJust screenGetDefault
	x <- screenGetWidth scr
	y <- screenGetHeight scr
	windowSetDefaultSize w x y
	windowFullscreen w
	on w deleteEvent $ liftIO $ exitWith ExitSuccess
	pure w

handleNewTurn conn w = do
	view <- fmap translateUniverse $ getView conn
	so <- newIORef Nothing
	usw <- newIORef Nothing
	pendingActions <- newTVarIO mempty
	uiState <- newTVarIO $ UIState
		{ galaxyDisplayOffsets = (0, 0)
		, selectedObject = so
		, updateShipWindow = usw
		, redrawStarlaneLayer = pure ()
		}
	postGUISync $ do
		-- purge the old window contents
		oldChild <- binGetChild w
		maybe (pure ()) (containerRemove w) oldChild
		-- and user's orders for the previous turn
		atomically $ writeTVar pendingActions []

		-- create an universe view window
		topPaned <- vPanedNew
		containerAdd w topPaned

		topPanel <- hBoxNew False 0
		panedPack1 topPaned topPanel False False

		turnLabel <- labelNew $ Just ("Turn " ++ (show $ turnNumber view))
		boxPackStart topPanel turnLabel PackNatural 0

		readyButton <- checkButtonNew
		readyButtonLabel <- labelNew $ Just ("Ready" :: Text)
		containerAdd readyButton readyButtonLabel
		boxPackStart topPanel readyButton PackNatural 0
		on readyButton buttonActivated $ do
			gotReady <- toggleButtonGetActive readyButton
			readyToAct <- widgetGetSensitive readyButton
			when (gotReady && readyToAct) $ do
				readyButton & widgetSetSensitive $ False
				labelSetText readyButtonLabel ("sending..." :: Text)
				readyButton & toggleButtonSetActive $ False
				toggleButtonSetInconsistent readyButton True
				readTVarIO pendingActions >>= print
				readTVarIO pendingActions >>= setActions conn
				-- after successful action submission
				toggleButtonSetInconsistent readyButton False
				labelSetText readyButtonLabel ("Ready" :: Text)
				readyButton & toggleButtonSetActive $ True
				readyButton & widgetSetSensitive $ True
		let adjustActions = adjustPendingActions (readyButton & toggleButtonSetActive $ False) pendingActions conn

		panels <- vPanedNew
		panedPack2 topPaned panels True True

		infoLabel <- labelNew (Nothing :: Maybe Text)
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
		atomically $ modifyTVar' uiState $ \s -> s { redrawStarlaneLayer = widgetQueueDraw starlaneLayer }

		layout <- fixedNew
		overlayAdd overlay layout	-- put it over the starlane map

		-- make overlay respect the size of the layout
		sg <- sizeGroupNew SizeGroupBoth
		sizeGroupAddWidget sg starlaneLayer
		sizeGroupAddWidget sg layout

		let annotatedView = annotateUniverseView view

		-- draw our view content
		addStarSystems view adjustActions uiState layout (labelSetText infoLabel . show) $ M.elems $ mapStarSystems view
		mapM_ (addFleets uiState layout (\s@Fleet {..} -> do
				let setShipInfo label Fleet {..} = do
					let showShipInfo = T.concat [T.pack $ show (length fleetShips), " ships owned by ", maybe "noone" empireName fleetOwner]
					labelSetText label showShipInfo
				setShipInfo infoLabel s
			)) $ groupBy ((==) `F.on` (\Fleet {..} -> fleetLocation)) $ fleets annotatedView
		UIState { galaxyDisplayOffsets = offsets } <- readTVarIO uiState
		it <- iconThemeGetDefault
		(Just crownPix) <- iconThemeLoadIcon it ("crown" :: Text) 16 IconLookupGenericFallback
		on starlaneLayer draw $ do
			drawLanes offsets view
			drawShipMoveOrders offsets view pendingActions
			drawSystemIdentifiers crownPix offsets view

		widgetShowAll w

main = do
	-- getArgs
	[cert, key] <- unsafeInitGUIForThreadedRTS
	missingAuthData <- fmap (not . and) $ sequence $ map doesFileExist [key, cert]
	when missingAuthData $ makeKeyCert key cert

	SDL.Init.initialize [InitAudio]
	-- 22050Hz by default, seriously!?
	let audio = def { audioFrequency = max 44100 $ audioFrequency def }
	openAudio audio 16384

	-- request a dark theme variant
	sets <- fmap fromJust $ settingsGetDefault
	settingsSetLongProperty sets ("gtk-application-prefer-dark-theme" :: Text) 1 mempty
	-- permit putting images on buttons
	settingsSetLongProperty sets ("gtk-button-images" :: Text) 1 mempty

	-- tell gtk where to look for icons
	iconTheme <- iconThemeGetDefault
	iconThemePrependSearchPath iconTheme "resources"

	let host = "localhost"
	forkIO $ turnWaiter host key cert
	mainGUI
