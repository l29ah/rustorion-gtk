{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}
{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, FlexibleInstances, DataKinds, TypeApplications, FlexibleContexts,MultiParamTypeClasses, TypeSynonymInstances #-}
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

import Data.IORef
import Data.Map
import Data.Text as T
import Data.Word
import GHC.Generics
import Data.Function hiding (id)
import Prelude hiding (id)

import RPC.Types (ID(..), Color(..), UniverseLocation(..), Action(..))
import qualified RPC.Types as RPC

data Planet = Planet
	{ id :: ID RPC.Planet
	, starSystem :: Maybe StarSystem
	} deriving (Generic)

data Empire = Empire
	{ id :: ID RPC.Empire
	, name :: Text
	, color :: Color
	, capital :: Maybe StarSystem
	} deriving (Generic)
instance Eq Empire where
	(==) = on (==) (.id)

data StarSystem = StarSystem
	{ id :: ID RPC.StarSystem
	, name :: Text
	, location :: UniverseLocation
	, population :: Word64
	, canCapture :: Bool
	, ships :: [Ship]
	, lanes :: [StarSystem]
	, owner :: Maybe Empire
	, isAdjacent :: StarSystem -> Bool
	} deriving (Generic)
instance Eq StarSystem where
	a == b = id (a :: StarSystem) == id (b :: StarSystem)
instance Show StarSystem where
	show a = T.unpack $ name (a :: StarSystem)

data Ship = Ship
	{ id :: ID RPC.Ship
	, name :: Text
	, location :: Maybe StarSystem
	, owner :: Maybe Empire
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
	{ ships :: [Ship]
	, location :: StarSystem
	, owner :: Maybe Empire
	}

data UIState = UIState
	{ galaxyDisplayOffsets :: (Double, Double)
	, redrawStarlaneLayer :: IO ()
	, selectedObject :: IORef (Maybe Fleet)
	, updateShipWindow :: IORef (Maybe (Ship -> IO ()))
	} deriving (Generic)
