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

instance Named (Galaxy a) where
  name = galaxyname

instance Named (StarSystem a) where
  name = starsystemname

instance Named (Star a) where
  name = starname

instance Named (Planet a) where
  name = planetname


