module Civilization
where

import qualified Data.Map as M
import Data.Maybe

import Galaxy
import Math
import ZipperGalaxy
import Named

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

data Terrain = Terrain { terraingoods :: [Resource] }
    deriving (Eq)

type Resource = (Good, Int)

instance Show Terrain where
  show t = "    " ++ concatMap ((++ " ") . show) (terraingoods t)

data Building = Building { buildingname :: String
                         , goodsToBuild :: [(Good, Int)]
                         , buildWork    :: Flt
                         , prerequisiteBuildings :: [Building]
                         , market :: Flt -- [0..1]
                         , productionbonus :: [(Good, Flt)] -- [0..1] for each good
                         }

instance Show Building where
  show = name

data Civilization = Civilization { civname :: String
                                 , settlements :: [Settlement]
                                 }

instance Show Civilization where
  show = name

data Settlement = Settlement { settlementzipper :: GalaxyZipper Terrain }

instance Show Settlement where
  show = name . fromJust . satelliteInZipper . settlementzipper

data Ruleset = Ruleset { goods     :: M.Map String Good
                       , buildings :: M.Map String Building
                       }
    deriving (Show)

instance Named Good where
  name = goodname

instance Named Building where
  name = buildingname

instance Named Civilization where
  name = civname

stdRules = Ruleset stdGoodsMap stdBuildingsMap

stdGoodsMap = namedsToMap stdGoods

stdBuildingsMap = namedsToMap stdBuildings

namedsToMap :: (Named a) => [a] -> M.Map String a
namedsToMap ns = M.fromList (zip (map name ns) ns)

--             Name            Natural             temp      atm        growth  initial needed goods   work building food bonus    lux med  weap
wood    = Good "Wood"         (Just $ Natural [WaterWeatherSystem] 1.0 1.0)  []             1.0 Nothing     0    []     0   0    0.01
animals = Good "Live animals" (Just $ Natural [WaterWeatherSystem] 0.5 0.5)  []             1.0 Nothing     1.0  []     0   0.01 0
water   = Good "Water"        (Just $ Natural [WaterWeatherSystem] 1.0 1.0)  []             0.1 Nothing     0    []     0   0.02 0
grain   = Good "Grain"        Nothing                                                   [water]        1.0 (Just farm) 1.0  []     0   0.01 0
iron    = Good "Iron"         (Just $ Natural []                   0.0 0.1)  []             2.0 (Just mine) 0    []     0   0    0.02
coal    = Good "Coal"         (Just $ Natural []                   0.0 0.05) []             2.0 (Just mine) 0    []     0   0    0
uranium = Good "Uranium"      (Just $ Natural []                   0.0 0.01) []             3.0 (Just mine) 0    []     0   0    0
oil     = Good "Oil"          (Just $ Natural []                   0.0 0.01) []             3.0 (Just mine) 0    []     0   0    0
gems    = Good "Gems"         (Just $ Natural []                   0.0 0.01) []             3.0 (Just mine) 0    []     0   0    0

stdGoods = [wood, animals, water, grain, iron, coal, uranium, oil, gems]

--                 Name       goods       work prereq market bonus
farm    = Building "Farm"     [(wood, 20)] 15  []     0      []
mine    = Building "Mine"     [(wood, 30)] 30  []     0      []

stdBuildings = [farm, mine]

