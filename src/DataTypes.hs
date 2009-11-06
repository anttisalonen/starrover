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

data Civilization a = Civilization { civname :: String
                                   , settlements :: [Settlement a]
                                   }
    deriving (Eq, Read, Show)

data Settlement a = Settlement { settlementplanet :: Planet a
                               , settlementstar :: Star a }
    deriving (Eq, Read, Show)

data Planet a = Planet { planetname :: String
                       , orbit :: Orbit
                       , physics :: BodyPhysics
                       , atmosphere :: Atmosphere
                       , satellites :: [Planet a]
                       , info :: a
                       }
    deriving (Eq, Read, Show)

data SpectralType = SpectralTypeB
                  | SpectralTypeA
                  | SpectralTypeF
                  | SpectralTypeG
                  | SpectralTypeK
                  | SpectralTypeM
    deriving (Eq, Read, Show, Enum, Bounded)

type Vector3 = (Flt, Flt, Flt)

data Star a = Star { starname :: String
                   , temperature :: Temperature
                   , starorbit :: Orbit
                   , planets :: [Planet a]
                   }
    deriving (Eq, Read, Show)

data StarSystem a = StarSystem { starsystemname :: String
                               , ssposition :: Vector3 
                               , stars :: [Star a]
                               }
    deriving (Eq, Read, Show)

data Galaxy a = Galaxy { galaxyname :: String
                       , starsystems :: [StarSystem a]
                       }
    deriving (Eq, Read, Show)

class Named a where
  name :: a -> String

instance Named (Galaxy a) where
  name = galaxyname

instance Named (StarSystem a) where
  name = starsystemname

instance Named (Star a) where
  name = starname

instance Named (Planet a) where
  name = planetname


