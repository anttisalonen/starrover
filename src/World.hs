module World
where

import Galaxy
import ZipperGalaxy
import ZipperGalaxyUtils
import DataFunction
import Civilization
import Statistics
import Utils
import System.Random
import Control.Monad.State
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import qualified Data.Edison.Assoc.StandardMap as E

data World = World { galaxy  :: Galaxy Terrain
                   , time    :: GalaxyTime
                   , empires :: E.FM CivKey Empire
                   , player  :: Player
                   }

data GalaxyTime = GalaxyTime { year   :: Int
                             , day    :: Int
                             , hour   :: Int
                             , minute :: Int
                             }

data Relation a = Relation { relationship :: Int
                           , contracts :: [Contract a] }

data Contract a = Embargo a

data Player = Player {playername :: String }

testRandomWorld :: Galaxy Terrain -> Int -> Int -> Maybe World
testRandomWorld g v numciv =
  let r = mkStdGen v
  in evalState (createWorld g numciv) r

nullGalaxyTime = GalaxyTime 0 0 0 0

nullPlayer = Player ""

habitablePlanetsZ :: Galaxy Terrain -> [GalaxyZipper Terrain]
habitablePlanetsZ g = filter sustainsLifeZ (ZipperGalaxyUtils.allBodies g)

civnames :: [String]
civnames = ["humans",
            "aliens",
            "ant insect animals",
            "slimy aliens",
            "cosmic hive snakes"
           ]

colonyStartPopulation :: ResourceUnit
colonyStartPopulation = 100

createWorld :: Galaxy Terrain -> Int -> Rnd (Maybe World)
createWorld g numciv = do
  let ps = habitablePlanetsZ g
  if null ps 
    then return Nothing
    else do
      ps' <- shuffle ps
      let cs = take numciv ps'
      if length cs < numciv 
        then return Nothing
        else do
          let cols = map ((:[]) . Colony "colony" colonyStartPopulation) (map zipperToNames cs)
          let empires = zipWith Empire civnames (map namedsToMap cols)
          let collocs = pairToLists civnames (repeat "colony")
          return $ Just $ World 
               (foldl' (\g' (cl, pn) -> setColonyOnPlanet cl pn g') 
                       g
                       (zip collocs (map planetname (mapMaybe satelliteInZipper cs))))
               nullGalaxyTime 
               (namedsToMap empires) 
               nullPlayer

data EmpireZipper = EmpireZipper { empiresInZipper  :: (E.FM CivKey Empire)
                                 , coloniesInZipper :: (Maybe (Name, E.FM CivKey Colony))
                                 }

newEmpireZipper :: World -> EmpireZipper
newEmpireZipper w = EmpireZipper (empires w) Nothing

upEZ :: EmpireZipper -> EmpireZipper
upEZ z = z

tryDownEZ :: String -> EmpireZipper -> Maybe EmpireZipper
tryDownEZ s z = Nothing

