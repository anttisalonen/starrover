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
import qualified Data.Map as M
import qualified Data.Edison.Assoc.StandardMap as E

data World = World { galaxy  :: Galaxy Terrain
                   , time    :: GalaxyTime
                   , empires :: M.Map Int Empire
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
          let ns = [1..]
          let enames = (map show [1..])
          let collocs = map zipperToNames ps
          let cols = map (Colony "colony") collocs
          let empires = zipWith Empire enames [cols]
          return $ Just $ World g nullGalaxyTime (M.fromList (zip ns empires)) nullPlayer

