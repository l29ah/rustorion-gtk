{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DuplicateRecordFields, RecordWildCards #-}

import Control.Concurrent
import qualified Data.Map as M
import Graphics.UI.Gtk
import System.Environment

import RPC
import Types

scale_factor = 20
scale x = round $ (x + 5000) / scale_factor

addStarSystem :: Layout -> StarSystem -> IO ()
addStarSystem layout ss@StarSystem {..} = do
	butt <- buttonNewWithLabel $ name
	let (UniverseLocation x y) = location
	layoutPut layout butt (scale x) (scale y)

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
		windowSetDefaultSize w 500 500

		universeScroll <- scrolledWindowNew Nothing Nothing
		scrolledWindowSetPolicy universeScroll PolicyAutomatic PolicyAutomatic
		containerAdd w universeScroll

		layout <- layoutNew Nothing Nothing
		containerAdd universeScroll layout
		layoutSetSize layout (round $ 10000 / scale_factor) (round $ 10000 / scale_factor)

		mapM_ (addStarSystem layout) $ M.elems $ star_systems view

		widgetShowAll w
