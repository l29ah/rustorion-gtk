{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, FlexibleInstances #-}
module Types
	( Color(..)
	, ID(..)
	, UniverseLocation(..)
	, Action(..)
	, Planet(..)
	, Empire(..)
	, StarSystem(..)
	, Ship(..)
	, UniverseView(..)
	, UIState(..)
	, Fleet(..)
	, AnnotatedUniverseView(..)
	) where

import Data.Function
import Data.IORef
import Data.Map
import Data.Text as T
import Data.Word
import GHC.Generics

import RPC.Types (ID(..), Color(..), UniverseLocation(..), Action(..))
import qualified RPC.Types as RPC

data Planet = Planet
	{ planetID :: ID RPC.Planet
	, planetStarSystem :: Maybe StarSystem
	} deriving (Generic)

data Empire = Empire
	{ empireID :: ID RPC.Empire
	, empireName :: Text
	, empireColor :: Color
	, empireCapital :: Maybe StarSystem
	} deriving (Generic)
instance Eq Empire where
	(==) = on (==) empireID

data StarSystem = StarSystem
	{ starSystemID :: ID RPC.StarSystem
	, starSystemName :: Text
	, starSystemLocation :: UniverseLocation
	, starSystemPopulation :: Word64
	, starSystemCanCapture :: Bool
	, starSystemShips :: [Ship]
	, starSystemLanes :: [StarSystem]
	, starSystemOwner :: Maybe Empire
	, isAdjacent :: StarSystem -> Bool
	} deriving (Generic)
instance Eq StarSystem where
	(==) = on (==) starSystemID
instance Show StarSystem where
	show = T.unpack . starSystemName

data Ship = Ship
	{ shipID :: ID RPC.Ship
	, shipName :: Text
	, shipLocation :: Maybe StarSystem
	, shipOwner :: Maybe Empire
	} deriving (Generic)

data UniverseView = UniverseView
	{ planets :: [Planet]
	, mapPlanets :: Map (ID RPC.Planet) Planet
	, starSystems :: [StarSystem]
	, mapStarSystems :: Map (ID RPC.StarSystem) StarSystem
	, empires :: [Empire]
	, mapEmpires :: Map (ID RPC.Empire) Empire
	, ships :: [Ship]
	, mapShips :: Map (ID RPC.Ship) Ship
	, controlledEmpire :: Maybe Empire
	, turnNumber :: Word
	} deriving (Generic)

data AnnotatedUniverseView = AnnotatedUniverseView
	{ view :: UniverseView
	, fleets :: [Fleet]
	}

data Fleet = Fleet
	{ fleetShips :: [Ship]
	, fleetLocation :: StarSystem
	, fleetOwner :: Maybe Empire
	}

data UIState = UIState
	{ galaxyDisplayOffsets :: (Double, Double)
	, redrawStarlaneLayer :: IO ()
	, selectedObject :: IORef (Maybe Fleet)
	, updateShipWindow :: IORef (Maybe (Ship -> IO ()))
	} deriving (Generic)
