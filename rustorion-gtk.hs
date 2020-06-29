{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import System.Environment

import RPC
import Types

main = do
	[key, cert] <- getArgs
	let host = "localhost"
	let port = 4433
	conn <- rpcConnect host port key cert
	view <- getView conn
	print view
