{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DuplicateRecordFields, RecordWildCards #-}

import Control.Concurrent
import Data.IORef
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import System.Environment

import RPC
import Types

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
scaleCoord x = round $ (x + 5000) / scaleFactor

addShip :: Fixed -> UniverseView -> (Ship -> IO ()) -> Ship -> IO ()
addShip layout view onClick ship@Ship { name = name, uuid = shid } = do
	butt <- buttonNewWithLabel $ name
	set butt [ widgetOpacity := 0.7 ]
	on butt buttonActivated $ onClick ship
	let ssid = (to $ ships_in_star_systems view) ! shid
	let (UniverseLocation x y) = location $ (star_systems view) ! ssid
	let shipButtonYOffset = 25
	fixedPut layout butt ((scaleCoord x), (shipButtonYOffset + (scaleCoord y)))

addStarSystem :: Fixed -> (StarSystem -> IO ()) -> StarSystem -> IO ()
addStarSystem layout onClick ss@StarSystem {..} = do
	butt <- buttonNewWithLabel $ name
	set butt [ widgetOpacity := 0.7 ]
	on butt buttonActivated $ onClick ss
	let (UniverseLocation x y) = location
	-- place the buttons in the layout so they can be realized
	fixedPut layout butt ((scaleCoord x), (scaleCoord y))
	after butt realize $ do
		-- center the star system buttons on their locations
		(Rectangle x y w h) <- widgetGetAllocation butt
		fixedMove layout butt (x - (w `div` 2), y - (h `div` 2))
	pure ()

drawLane (UniverseLocation x1 y1) (UniverseLocation x2 y2) = do
	setLineWidth 2
	setSourceRGB 0 1 0
	moveTo (fromIntegral $ scaleCoord x1) (fromIntegral $ scaleCoord y1)
	lineTo (fromIntegral $ scaleCoord x2) (fromIntegral $ scaleCoord y2)
	stroke

drawLanes UniverseView {..} = do
	-- black interstellar space
	setSourceRGB 0 0 0
	paint

	mapM_ (\(id1, ids) -> mapM_ (\id2 ->
				drawLane (location $ star_systems ! id1) (location $ star_systems ! id2)
			) ids
		) $ M.toList starlanes

main = do
	[key, cert] <- getArgs
	let host = "localhost"
	let port = 4433
	conn <- rpcConnect host port key cert
	view <- getView conn
	print view
	forkIO $ do
		unsafeInitGUIForThreadedRTS
		mainGUI
	postGUISync $ do
		-- request a dark theme variant
		sets <- settingsGetDefault
		settingsSetLongProperty (fromJust sets) ("gtk-application-prefer-dark-theme" :: String) 1 []

		w <- windowNew
		set w [windowTitle := ("rustorion-gtk" :: String)]

		-- or better https://askubuntu.com/questions/153549/how-to-detect-a-computers-physical-screen-size-in-gtk
		scr <- fmap fromJust screenGetDefault
		x <- screenGetWidth scr
		y <- screenGetHeight scr
		windowSetDefaultSize w x y
		windowFullscreen w

		panels <- vPanedNew
		containerAdd w panels

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
		on starlaneLayer draw $ drawLanes view
		containerAdd overlay starlaneLayer

		layout <- fixedNew
		overlayAdd overlay layout	-- put it over the starlane map

		-- make overlay respect the size of the layout
		sg <- sizeGroupNew SizeGroupBoth
		sizeGroupAddWidget sg starlaneLayer
		sizeGroupAddWidget sg layout

		mapM_ (addStarSystem layout (labelSetText infoLabel . show)) $ M.elems $ star_systems view
		mapM_ (addShip layout view (labelSetText infoLabel . show)) $ M.elems $ ships view

		widgetShowAll w
