{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module Galaxy
where

import qualified Data.Edison.Assoc.StandardMap as E

import Math
import Libaddutil.Named
import Utils

type Temperature = Int

data PlanetType = Planetoid
                | NoAtmosphere
                | RockyPlanet Atmosphere
                | SmallGasGiant
                | MediumGasGiant
                | LargeGasGiant
                | VeryLargeGasGiant
    deriving (Eq, Read, Show)

data Atmosphere = Nitrogen
                | CarbonDioxide
                | WaterWeatherSystem
                | MethaneWeatherSystem
                | SulphurDioxide
    deriving (Eq, Read, Show, Enum, Bounded)

data Orbit = Orbit { orbitradius :: Flt
                   , initialpos :: Flt
                   , eccentricity :: Flt
                   , orbitspeed :: Flt
                   , ztilt :: Flt
                   }
    deriving (Eq, Read, Show)

data BodyPhysics = BodyPhysics { bodymass :: Flt
                               }
    deriving (Eq, Read, Show)

data Planet a = Planet { planetname :: String
                       , orbit :: Orbit
                       , physics :: BodyPhysics
                       , planettype :: PlanetType
                       , planettemperature :: Temperature
                       , satellites :: E.FM Name (Planet a)
                       , info :: a
                       }
    deriving (Eq, Read, Show)

data SpectralType = SpectralTypeB
                  | SpectralTypeA
                  | SpectralTypeF
                  | SpectralTypeG
                  | SpectralTypeK
                  | SpectralTypeM
    deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Star a = Star { starname :: String
                   , temperature :: Temperature
                   , starorbit :: Orbit
                   , planets :: E.FM Name (Planet a)
                   }
    deriving (Eq, Read, Show)

data StarSystem a = StarSystem { starsystemname :: String
                               , ssposition :: Vector3 
                               , stars :: E.FM Name (Star a)
                               }
    deriving (Eq, Read, Show)

data Galaxy a = Galaxy { galaxyname :: String
                       , starsystems :: E.FM Name (StarSystem a)
                       }
    deriving (Eq, Read, Show)

class TreeContainer a k v where
  content :: a -> E.FM k v

instance TreeContainer (Planet a) Name (Planet a) where
  content = satellites

instance TreeContainer (Star a) Name (Planet a) where
  content = planets

instance TreeContainer (StarSystem a) Name (Star a) where
  content = stars

instance TreeContainer (Galaxy a) Name (StarSystem a) where
  content = starsystems

instance Named (Galaxy a) where
  name = galaxyname

instance Named (StarSystem a) where
  name = starsystemname

instance Named (Star a) where
  name = starname

instance Named (Planet a) where
  name = planetname

class SpecificContainer a b k where
  adj :: k -> (b -> b) -> a -> a
  smap :: (b -> b) -> a -> a

instance SpecificContainer (Planet a) (Planet a) Name where
  adj k f g = g{satellites = E.adjust f k (content g)}
  smap f g = g{satellites = E.map f (satellites g)}

instance SpecificContainer (Star a) (Planet a) Name where
  adj k f g = g{planets = E.adjust f k (planets g)}
  smap f g = g{planets = E.map f (planets g)}

instance SpecificContainer (StarSystem a) (Star a) Name where
  adj k f g = g{stars = E.adjust f k (stars g)}
  smap f g = g{stars = E.map f (stars g)}

instance SpecificContainer (Galaxy a) (StarSystem a) Name where
  adj k f g = g{starsystems = E.adjust f k (starsystems g)}
  smap f g = g{starsystems = E.map f (starsystems g)}

updateSatellite :: Name -> (Planet a -> Planet a) -> Planet a -> Planet a
updateSatellite = adj

updatePlanet :: Name -> (Planet a -> Planet a) -> Star a -> Star a
updatePlanet = adj

updateStar :: Name -> (Star a -> Star a) -> StarSystem a -> StarSystem a
updateStar = adj

updateStarsystem :: Name -> (StarSystem a -> StarSystem a) -> Galaxy a -> Galaxy a
updateStarsystem = adj

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


