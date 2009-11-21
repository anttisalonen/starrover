{-# LANGUAGE MultiParamTypeClasses #-}
module World
where

import Galaxy
import Civilization
import Statistics
import System.Random
import Control.Monad.State

data World = World { galaxy  :: Galaxy Terrain
                   , time    :: GalaxyTime
                   , empires :: [Empire]
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

class TreeContainer a b k c where
  content :: a b k c -> b
  child :: (TreeContainer x y z w) => a b k c -> k -> x y z w

data Player = Player {playername :: String }

testRandomWorld :: Galaxy Terrain -> Int -> Int -> Maybe World
testRandomWorld g v numciv =
  let r = mkStdGen v
  in Just $ evalState (createWorld g numciv) r

nullGalaxyTime = GalaxyTime 0 0 0 0

nullPlayer = Player ""

createWorld :: Galaxy Terrain -> Int -> Rnd World
createWorld g numciv = do
  return $ World g nullGalaxyTime [] nullPlayer
