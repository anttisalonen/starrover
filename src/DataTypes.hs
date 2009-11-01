module DataTypes
where

data Good = Good { goodname :: String }
    deriving (Eq, Read, Show)

data Terrain = Terrain { goods :: [Good] }
    deriving (Eq, Read, Show)

type Flt = Float

type Temperature = Int

data Atmosphere = NoAtmosphere
                | Nitrogen
                | CarbonDioxide
                | Oxygen
                | Methane
                | GasGiant
    deriving (Eq, Read, Show, Enum, Bounded)

type Ressource = (Good, Int)

data Building = Building { buildingname :: String }
    deriving (Eq, Read, Show)

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

data Planet = Planet { planetname :: String
                     , orbit :: Orbit
                     , physics :: BodyPhysics
                     , atmosphere :: Atmosphere
                     , satellites :: [Planet]
                     }
    deriving (Eq, Read, Show)

data SpectralType = SpectralTypeO
                  | SpectralTypeB
                  | SpectralTypeA
                  | SpectralTypeF
                  | SpectralTypeG
                  | SpectralTypeK
                  | SpectralTypeM
    deriving (Eq, Read, Show, Enum, Bounded)

type Vector3 = (Flt, Flt, Flt)

data Star = Star { starname :: String
                 , temperature :: Temperature
                 , starorbit :: Orbit
                 , planets :: [Planet]
                 }
    deriving (Eq, Read, Show)

data StarSystem = StarSystem { ssname :: String
                             , ssposition :: Vector3 
                             , stars :: [Star]
                             }
    deriving (Eq, Read, Show)

data Galaxy = Galaxy { galaxyname :: String
                     , starsystems :: [StarSystem]
                     }
    deriving (Eq, Read, Show)

