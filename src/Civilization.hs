module Civilization
where

import qualified Data.Map as M

import ZipperGalaxy

data Good = Good { goodname :: String }
    deriving (Eq, Read, Show)

data Terrain = Terrain { goods :: [Good] }
    deriving (Eq, Read, Show)

type Ressource = (Good, Int)

data Building = Building { buildingname :: String }
    deriving (Eq, Read, Show)

data Civilization a = Civilization { civname :: String
                                   , settlements :: [Settlement a]
                                   }
    deriving (Eq, Read, Show)

data Settlement a = Settlement { settlementzipper :: GalaxyZipper a }
    deriving (Eq, Read, Show)


