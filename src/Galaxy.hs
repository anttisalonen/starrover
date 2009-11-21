module Galaxy
where

import qualified Data.Map as M

import Math
import Libaddutil.Named

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
                       , satellites :: M.Map Int (Planet a)
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
                   , planets :: M.Map Int (Planet a)
                   }
    deriving (Eq, Read, Show)

data StarSystem a = StarSystem { starsystemname :: String
                               , ssposition :: Vector3 
                               , stars :: M.Map Int (Star a)
                               }
    deriving (Eq, Read, Show)

data Galaxy a = Galaxy { galaxyname :: String
                       , starsystems :: M.Map Int (StarSystem a)
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


