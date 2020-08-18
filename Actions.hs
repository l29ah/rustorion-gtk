{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, FlexibleInstances, DataKinds, TypeApplications, FlexibleContexts,MultiParamTypeClasses, TypeSynonymInstances, RecordWildCards #-}
module Actions where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Network.Connection
import Prelude hiding (id)
import Data.Function hiding (id)

import RPC
import RPC.Types as RPCT
import Types as My

dontCaptureStarSystem :: ID RPCT.StarSystem -> [Action] -> [Action]
dontCaptureStarSystem ssid = filter (\act ->
	case act of
		CaptureStarSystem s _ -> ssid /= s
		MoveShip _ _ -> True
	)

captureStarSystem :: ID RPCT.StarSystem -> ID RPCT.Empire -> [Action] -> [Action]
captureStarSystem ssid eid actions = CaptureStarSystem ssid eid : dontCaptureStarSystem ssid actions

dontMoveShip :: ID RPCT.Ship -> [Action] -> [Action]
dontMoveShip shid = filter (\act ->
	case act of
		CaptureStarSystem _ _ -> True
		MoveShip s _ -> shid /= s
	)

moveShip :: ID RPCT.Ship -> ID RPCT.StarSystem -> [Action] -> [Action]
moveShip shid ssid actions = MoveShip shid ssid : dontMoveShip shid actions

apply = foldl1 (.)

dontMoveFleet :: My.Fleet -> [Action] -> [Action]
dontMoveFleet My.Fleet {..} = apply (map dontMoveShip $ map (.id) ships)

moveFleet :: My.Fleet -> ID RPCT.StarSystem -> [Action] -> [Action]
moveFleet fleet ssid = apply (map (\ship -> moveShip (ship.id) ssid) fleet.ships)

adjustPendingActions :: IO () -> TVar [Action] -> MVar Connection -> ([Action] -> [Action]) -> IO ()
adjustPendingActions untick actionVar conn modifier = do
	untick
	cancelActions conn
	atomically $ modifyTVar' actionVar modifier
