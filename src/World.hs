module World
where

import DataTypes

data World = World { galaxy  :: Galaxy
                   , time    :: GalaxyTime
                   , nations :: [Nation]
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

data Player = Player {playername :: String }
