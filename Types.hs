{-# LANGUAGE DeriveGeneric #-}
module Types where

import Data.Map
import Data.MessagePack as MP
import Data.Void
import Data.Word
import GHC.Generics

newtype ID a = ID Word64 deriving (Show, Eq, Ord, Generic)
instance MessagePack (ID a)

data UniverseView = UniverseView
	{ planets :: Map (ID Void) Object
	, star_systems :: Object
	, current_id :: Object
	, empires :: Object
	, planets_in_star_systems :: Object
	, starlanes :: Object
	, star_systems_in_empires :: Object
	, ships :: Object
	, ships_in_star_systems :: Object
	, ships_in_empires :: Object
	, controlled_empire :: Object
	, turn_number :: Word
	} deriving (Show, Eq, Generic)

instance MessagePack UniverseView
