{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, FlexibleInstances, DisambiguateRecordFields #-}
module RPC.Types where

import Data.Default
import Data.Map
import Data.MessagePack as MP
import Data.Text
import Data.Void
import Data.Word
import GHC.Generics

newtype ID a = ID Word64 deriving (Show, Eq, Ord, Generic)
instance MessagePack (ID a)

data Color = Color
	{ r :: Float
	, g :: Float
	, b :: Float
	} deriving (Show, Eq, Ord, Generic)
instance MessagePack Color
instance Default Color where
	def = Color 0.5 0.5 0.5

data UniverseLocation = UniverseLocation
	{ ulx :: Double
	, uly :: Double
	} deriving (Show, Eq, Ord, Generic)
instance MessagePack UniverseLocation

data Planet = Planet
	{ uuid :: ID Planet
	} deriving (Show, Eq, Ord, Generic)
instance MessagePack Planet

instance {-# OVERLAPPING #-} MessagePack (Maybe (ID a)) where
	fromObject ObjectNil = pure Nothing
	fromObject x = fromObject x >>= pure . Just

data Empire = Empire
	{ uuid :: ID Empire
	, name :: Text
	, color :: Color
	} deriving (Show, Eq, Ord, Generic)
instance MessagePack Empire

data StarSystem = StarSystem
	{ uuid :: ID StarSystem
	, name :: Text
	, location :: UniverseLocation
	, population :: Word64
	, can_capture :: Bool
	} deriving (Show, Eq, Ord, Generic)
instance MessagePack StarSystem

data Ship = Ship
	{ uuid :: ID Ship
	, name :: Text
	} deriving (Show, Eq, Ord, Generic)
instance MessagePack Ship

data HasMany a b = HasMany
	{ from :: Map (ID a) ([ID b])
	, to :: Map (ID b) (ID a)
	} deriving (Show, Eq, Ord, Generic)
instance (MessagePack a, MessagePack b) => MessagePack (HasMany a b)

data UniverseView = UniverseView
	{ planets :: Map (ID Planet) Planet
	, star_systems :: Map (ID StarSystem) StarSystem
	, current_id :: ID Void
	, empires :: Map (ID Empire) Empire
	, planets_in_star_systems :: HasMany StarSystem Planet
	, starlanes :: Map (ID StarSystem) [ID StarSystem]
	, star_systems_in_empires :: HasMany Empire StarSystem
	, ships :: Map (ID Ship) Ship
	, ships_in_star_systems :: HasMany StarSystem Ship
	, ships_in_empires :: HasMany Empire Ship
	, capitals_in_empires :: Map (ID Empire) (ID StarSystem)
	, controlled_empire :: ID Empire
	, turn_number :: Word
	} deriving (Show, Eq, Generic)
instance MessagePack UniverseView

data Action =
	CaptureStarSystem (ID StarSystem) (ID Empire) |
	MoveShip (ID Ship) (ID StarSystem)
	deriving (Show, Eq, Generic)
instance MessagePack Action where
	toObject (CaptureStarSystem id1 id2) = ObjectMap [(ObjectWord 0, ObjectArray [toObject id1, toObject id2])]
	toObject (MoveShip id1 id2) = ObjectMap [(ObjectWord 1, ObjectArray [toObject id1, toObject id2])]
