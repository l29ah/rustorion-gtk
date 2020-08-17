{-# LANGUAGE DuplicateRecordFields, RecordWildCards, DisambiguateRecordFields #-}
module RPC.Translate
	( translateUniverse
	) where

import qualified Data.Map as M
import Data.Maybe

import qualified RPC.Types as RPCT
import Types
import Data.Function hiding (id)
import Prelude hiding (id)

translatePlanet :: UniverseView -> RPCT.UniverseView -> RPCT.Planet -> Planet
translatePlanet view rpctview RPCT.Planet {..} = Planet
	{ id = uuid
	, starSystem = do
		ssid <- M.lookup uuid $ RPCT.to $ RPCT.planets_in_star_systems rpctview
		M.lookup ssid $ mapStarSystems view
	}

translateStarSystem :: UniverseView -> RPCT.UniverseView -> RPCT.StarSystem -> StarSystem
translateStarSystem view rpctview RPCT.StarSystem {..} = StarSystem
	{ id = uuid
	, name = name
	, location = location
	, population = population
	, canCapture = can_capture
	, ships = concat $ map maybeToList $ map (\i -> M.lookup i $ mapShips view) $ concat $ maybeToList $ M.lookup uuid (RPCT.from $ RPCT.ships_in_star_systems rpctview)
	, owner = do
		eid <- M.lookup uuid $ RPCT.to $ RPCT.star_systems_in_empires rpctview
		M.lookup eid $ mapEmpires view
	, lanes = concatMap (\linkedSystem -> maybeToList $ M.lookup linkedSystem $ mapStarSystems view) $ concat $ maybeToList $ M.lookup uuid $ RPCT.starlanes rpctview
	, isAdjacent = \otherSystem -> elem (id (otherSystem :: StarSystem)) $ concat $ maybeToList $ M.lookup uuid $ RPCT.starlanes rpctview
	}

translateShip :: UniverseView -> RPCT.UniverseView -> RPCT.Ship -> Ship
translateShip view rpctview RPCT.Ship {..} = Ship
	{ id = uuid
	, name = name
	, location = do
		ssid <- M.lookup uuid $ RPCT.to $ RPCT.ships_in_star_systems rpctview
		M.lookup ssid $ mapStarSystems view
	, owner = do
		eid <- M.lookup uuid $ RPCT.to $ RPCT.ships_in_empires rpctview
		M.lookup eid $ mapEmpires view
	}

translateEmpire :: UniverseView -> RPCT.UniverseView -> RPCT.Empire -> Empire
translateEmpire view rpctview RPCT.Empire {..} = Empire
	{ id = uuid
	, name = name
	, color = color
	, capital = do
		ssid <- M.lookup uuid $ RPCT.capitals_in_empires rpctview
		M.lookup ssid $ mapStarSystems view
	}

translateUniverse :: RPCT.UniverseView -> UniverseView
translateUniverse view = result
	where result = UniverseView
		{ mapPlanets = M.map (translatePlanet result view) $ RPCT.planets view
		, mapStarSystems = M.map (translateStarSystem result view) $ RPCT.star_systems view
		, mapShips = M.map (translateShip result view) $ RPCT.ships view
		, mapEmpires = M.map (translateEmpire result view) $ RPCT.empires view

		, planets = M.elems $ mapPlanets result
		, starSystems = M.elems $ mapStarSystems result
		, empires = M.elems $ mapEmpires result
		, ships = M.elems $ mapShips result

		, controlledEmpire = do
			let eid = RPCT.controlled_empire view
			M.lookup eid $ mapEmpires result
		, turnNumber = RPCT.turn_number view
		}
