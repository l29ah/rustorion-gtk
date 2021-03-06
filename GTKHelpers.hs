{-# LANGUAGE ScopedTypeVariables #-}
module GTKHelpers where

import Control.Exception
import Graphics.UI.Gtk
import System.Exit

gtkTerminate = postGUIAsync mainQuit

terminateOnException :: IO a -> IO a
terminateOnException action = catch action $ \(e :: SomeException) -> gtkTerminate >> die (displayException e)
