module Civilization
where

import qualified Data.Map as M
import Data.Maybe
import Data.Tree

import Text.Printf
import Galaxy
import Math
import ZipperGalaxy
import Libaddutil.Named
import Utils
import qualified Data.Edison.Assoc.StandardMap as E

type CivKey = Name

type ResourceUnit = Integer

type GoodName = String

data Good = Good { goodname :: String
                 , natural :: Maybe Natural
                 , neededGoods :: [Good]
                 , work :: Flt
                 , neededBuilding :: Maybe Building
                 , food :: Flt   -- [0..1]
                 , machinery :: [(Good, Flt)]   -- [0..1] for each good
                 , luxuryValue :: Flt   -- [0..1]
                 , medicineValue :: Flt   -- [0..1]
                 , weaponValue :: Flt   -- [0..1]
                 }

instance Show Good where
  show = name

instance Eq Good where
  a == b = name a == name b

data Natural = Natural { neededAtmosphere :: [Atmosphere] -- OR'ed
                       , growthRate       :: Flt -- [0..1]
                       , initialAmount    :: Flt -- [0..1]
                       }
    deriving (Eq, Read, Show)

data Terrain = Terrain { terraingoods :: [(Resource, ResourceUnit)]
                       , colony       :: [Name] }
    deriving (Eq)

type Resource = (Good, ResourceUnit)

instance Show Terrain where
  show t = "    " ++ concatMap ((++ " ") . show) (terraingoods t)

data Building = Building { buildingname :: String
                         , goodsToBuild :: [(Good, ResourceUnit)]
                         , buildWork    :: Flt
                         , prerequisiteBuildings :: [Building]
                         , productionbonus :: [(Good, Flt)] -- [0..1] for each good
                         }

instance Show Building where
  show = name

data Empire = Empire { empirename :: String
                     , colonies   :: E.FM CivKey Colony
                     }

instance Show Empire where
  show = dispEmpire

dispEmpire :: Empire -> String
dispEmpire e = printf "%s - %d colonies" (name e) (E.size (colonies e))

dispColoniesInfo :: Empire -> String
dispColoniesInfo = concatMap dispColony . E.elements . colonies

type Inventory = E.FM GoodName ResourceUnit

transaction :: GoodName -> ResourceUnit -> Inventory -> Inventory
transaction g u i = E.adjust (+u) g i

stdColony :: String -> ResourceUnit -> GalaxyLocation -> Colony
stdColony n p l = Colony n p l E.empty

data Colony = Colony { colonyname :: String
                     , population :: ResourceUnit
                     , location   :: GalaxyLocation
                     , market     :: Inventory
                     }

dispColony :: Colony -> String
dispColony c = printf "\t%s - %s - %s" (name c) (show (population c)) (show (location c))

instance Show Colony where
  show = dispColony

data Ruleset = Ruleset { goods     :: M.Map String Good
                       , buildings :: M.Map String Building
                       }
    deriving (Show)

instance Named Colony where
  name = colonyname

instance Named Empire where
  name = empirename

instance Named Good where
  name = goodname

instance Named Building where
  name = buildingname

stdRules = Ruleset stdGoodsMap stdBuildingsMap

stdGoodsMap = namedsToMap stdGoods

stdBuildingsMap = namedsToMap stdBuildings

--             Name            Natural            atm        growth  initial needed goods   work building food bonus    lux med  weap
wood    = Good "Wood"         (Just $ Natural [WaterWeatherSystem] 1.0 1.0)  []             1.0 Nothing     0    []     0   0    0.01
animals = Good "Live animals" (Just $ Natural [WaterWeatherSystem] 0.5 0.5)  []             1.0 Nothing     1.0  []     0   0.01 0
water   = Good "Water"        (Just $ Natural [WaterWeatherSystem] 1.0 1.0)  []             0.1 Nothing     0    []     0   0.02 0
grain   = Good "Grain"        Nothing                                        [water]        1.0 (Just farm) 1.0  []     0   0.01 0
iron    = Good "Iron"         (Just $ Natural []                   0.0 0.1)  []             2.0 (Just mine) 0    []     0   0    0.02
coal    = Good "Coal"         (Just $ Natural []                   0.0 0.05) []             2.0 (Just mine) 0    []     0   0    0
uranium = Good "Uranium"      (Just $ Natural []                   0.0 0.01) []             3.0 (Just mine) 0    []     0   0    0
oil     = Good "Oil"          (Just $ Natural []                   0.0 0.01) []             3.0 (Just mine) 0    []     0   0    0
gems    = Good "Gems"         (Just $ Natural []                   0.0 0.01) []             3.0 (Just mine) 0    []     0   0    0

stdGoods :: [Good]
stdGoods = [wood, animals, water, grain, iron, coal, uranium, oil, gems]

--                 Name       goods       work prereq bonus
farm    = Building "Farm"     [(wood, 20)] 15  []     []
mine    = Building "Mine"     [(wood, 30)] 30  []     []

stdBuildings = [farm, mine]

regenerate :: Flt -> Terrain -> Terrain
regenerate coeff t = let nr = map (regenRes coeff) (terraingoods t)
                     in t{terraingoods = nr}

setColony :: EmpireLocation -> Terrain -> Terrain
setColony f t = t{colony = f}

regenRes :: Flt -> (Resource, ResourceUnit) -> (Resource, ResourceUnit)
regenRes coeff r@(_, m) = (regenerateGood coeff r, m)

regenerateGood :: Flt -> (Resource, ResourceUnit) -> Resource
regenerateGood coeff (r@(g, v), m) = 
  case natural g of
    Nothing -> r
    Just n  -> if v == 0 
                 then r
                 else if growthRate n == 0
                        then r
                        else (g, min m (floor (fromIntegral v * (1 + coeff * growthRate n))))

updateTerrain :: (a -> a) -> Planet a -> Planet a
updateTerrain f p = let oldinfo = info p
                        newinfo = f oldinfo
                    in p{info = newinfo}

updateSatellite :: Name -> (Planet a -> Planet a) -> Planet a -> Planet a
updateSatellite n f p = p{satellites = E.adjust f n (satellites p)}

updatePlanet :: Name -> (Planet a -> Planet a) -> Star a -> Star a
updatePlanet n f s = s{planets = E.adjust f n (planets s)}

updateStar :: Name -> (Star a -> Star a) -> StarSystem a -> StarSystem a
updateStar n f s = s{stars = E.adjust f n (stars s)}

updateStarsystem :: Name -> (StarSystem a -> StarSystem a) -> Galaxy a -> Galaxy a
updateStarsystem n f g = g{starsystems = E.adjust f n (starsystems g)}

updateStarsystems :: (StarSystem a -> StarSystem a) -> Galaxy a -> Galaxy a
updateStarsystems f g = g{starsystems = E.map f (starsystems g)}

updateStars :: (Star a -> Star a) -> StarSystem a -> StarSystem a
updateStars f s = s{stars = E.map f (stars s)}

updatePlanets :: (Planet a -> Planet a) -> Star a -> Star a
updatePlanets f s = s{planets = E.map f (planets s)}

updateSatellites :: (Planet a -> Planet a) -> Planet a -> Planet a
updateSatellites f p = p{satellites = E.map f (satellites p)}

updateBodies :: (Planet a -> Planet a) -> Star a -> Star a
updateBodies f s = let s' = updatePlanets f s
                       ps = E.map (updateSatellites f) (planets s')
                   in s'{planets = ps}

regenerateGalaxy :: Flt -> Galaxy Terrain -> Galaxy Terrain
regenerateGalaxy coeff = updateStarsystems $ updateStars $ updateBodies $ updateTerrain $ regenerate coeff

type EmpireLocation = [Name]

setColonyOnPlanet :: EmpireLocation -> Name -> Galaxy Terrain -> Galaxy Terrain
setColonyOnPlanet f n = updateStarsystems $ updateStars $ updateBodies $ (\p -> if name p == n then (updateTerrain (setColony f) p) else p)

terrainInfo :: E.FM CivKey Empire -> Terrain -> String
terrainInfo es t = show t ++
  case findColony es (colony t) of 
    Nothing         -> ""
    Just (emp, col) -> "\n\t" ++ show emp ++ show col

findColony :: E.FM CivKey Empire -> EmpireLocation -> Maybe (Empire, Colony)
findColony _ []     = Nothing
findColony m (e:es) = do
  n <- E.lookupM e m
  c <- findColony' (colonies n) es
  return (n, c)

findColony' :: E.FM CivKey Colony -> EmpireLocation -> Maybe Colony
findColony' _ []    = Nothing
findColony' m (e:_) = E.lookupM e m

