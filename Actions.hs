{-# LANGUAGE RecordWildCards #-}
module Actions where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Network.Connection
import Prelude hiding (id)
import Data.Function hiding (id)

import RPC
import RPC.Types as RPCT
import Types
import qualified Types as My

dontCaptureStarSystem :: ID StarSystem -> [Action] -> [Action]
dontCaptureStarSystem ssid = filter (\act ->
	case act of
		CaptureStarSystem s _ -> ssid /= s
		MoveShip _ _ -> True
	)

captureStarSystem :: ID StarSystem -> ID Empire -> [Action] -> [Action]
captureStarSystem ssid eid actions = CaptureStarSystem ssid eid : dontCaptureStarSystem ssid actions

dontMoveShip :: ID Ship -> [Action] -> [Action]
dontMoveShip shid = filter (\act ->
	case act of
		CaptureStarSystem _ _ -> True
		MoveShip s _ -> shid /= s
	)

moveShip :: ID Ship -> ID StarSystem -> [Action] -> [Action]
moveShip shid ssid actions = MoveShip shid ssid : dontMoveShip shid actions

apply = foldl1 (.)

dontMoveFleet :: Fleet -> [Action] -> [Action]
dontMoveFleet Fleet {..} = apply (map dontMoveShip $ map (id) ships)

moveFleet :: Fleet -> ID StarSystem -> [Action] -> [Action]
moveFleet f@Fleet {..} ssid = apply (map (\ship -> moveShip (id ship) ssid) ships) . dontMoveFleet f

adjustPendingActions :: IO () -> TVar [Action] -> MVar Connection -> ([Action] -> [Action]) -> IO ()
adjustPendingActions untick actionVar conn modifier = do
	untick
	cancelActions conn
	atomically $ modifyTVar' actionVar modifier
