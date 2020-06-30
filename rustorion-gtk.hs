{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DuplicateRecordFields, RecordWildCards #-}

import Control.Concurrent
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk
import System.Environment

import RPC
import Types

scaleFactor = 20
scaleCoord x = round $ (x + 5000) / scaleFactor

onShipClick ship = print ship
onStarSystemClick ss = print ss

addShip :: Fixed -> UniverseView -> Ship -> IO ()
addShip layout view ship@Ship { name = name, uuid = shid } = do
	butt <- buttonNewWithLabel $ name
	set butt [ widgetOpacity := 0.7 ]
	on butt buttonActivated $ onShipClick ship
	let ssid = (to $ ships_in_star_systems view) ! shid
	let (UniverseLocation x y) = location $ (star_systems view) ! ssid
	let shipButtonYOffset = 25
	fixedPut layout butt ((scaleCoord x), (shipButtonYOffset + (scaleCoord y)))

addStarSystem :: Fixed -> StarSystem -> IO ()
addStarSystem layout ss@StarSystem {..} = do
	butt <- buttonNewWithLabel $ name
	set butt [ widgetOpacity := 0.7 ]
	on butt buttonActivated $ onStarSystemClick ss
	let (UniverseLocation x y) = location
	fixedPut layout butt ((scaleCoord x), (scaleCoord y))

drawLane (UniverseLocation x1 y1) (UniverseLocation x2 y2) = do
	setLineWidth 2
	setSourceRGB 0 1 0
	moveTo (fromIntegral $ scaleCoord x1) (fromIntegral $ scaleCoord y1)
	lineTo (fromIntegral $ scaleCoord x2) (fromIntegral $ scaleCoord y2)
	stroke

drawLanes UniverseView {..} = do
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
		w <- windowNew
		set w [windowTitle := ("rustorion-gtk" :: String)]

		-- or better https://askubuntu.com/questions/153549/how-to-detect-a-computers-physical-screen-size-in-gtk
		scr <- fmap fromJust screenGetDefault
		x <- screenGetWidth scr
		y <- screenGetHeight scr
		windowSetDefaultSize w x y
		windowFullscreen w

		universeScroll <- scrolledWindowNew Nothing Nothing
		scrolledWindowSetPolicy universeScroll PolicyAutomatic PolicyAutomatic
		containerAdd w universeScroll

		-- TODO https://github.com/gtk2hs/gtk2hs/issues/295
		viewport <- viewportNew Nothing Nothing
		containerAdd universeScroll viewport

		overlay <- overlayNew
		containerAdd viewport overlay

		starlaneLayer <- drawingAreaNew
		on starlaneLayer draw $ drawLanes view
		containerAdd overlay starlaneLayer

		layout <- fixedNew
		overlayAdd overlay layout	-- put it over the starlane map

		-- make overlay respect the size of the layout
		sg <- sizeGroupNew SizeGroupBoth
		sizeGroupAddWidget sg starlaneLayer
		sizeGroupAddWidget sg layout

		mapM_ (addStarSystem layout) $ M.elems $ star_systems view
		mapM_ (addShip layout view) $ M.elems $ ships view

		widgetShowAll w
