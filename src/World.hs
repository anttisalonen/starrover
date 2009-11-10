{-# LANGUAGE MultiParamTypeClasses #-}
module World
where

import Galaxy

data World = World { galaxy  :: Galaxy
                   , time    :: GalaxyTime
                   , civilizations :: [Civilization]
                   , player  :: Player
                   }

data GalaxyTime = GalaxyTime { year   :: Int
                             , day    :: Int
                             , hour   :: Int
                             , minute :: Int
                             }

data Nation = Nation { nationname :: String
                     , federation :: Federation
                     , relations :: [(Nation, Relation)]
                     }

data Federation = Federation { states :: [State] }

data Relation = Relation { relationship :: Int
                         , contracts :: [Contract] }

data Contract = Embargo Nation

data State = State { colonies :: [Colony] }

data Colony = Colony { planet :: Planet }

class TreeContainer a b k c where
  content :: a b k c -> b
  child :: (TreeContainer x y z w) => a b k c -> k -> x y z w


data Player = Player {playername :: String }
