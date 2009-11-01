module DataCreate
where

import Data.Char (chr)
import Data.List (sort)
import Control.Monad.State
import System.Random

import DataTypes
import DataFunction

{-
moon :: Planet
moon = Planet "Moon" 0.015 0.8 1.0 0.0 0.0 10.0 NoAtmosphere []

earth :: Planet
earth = Planet "Earth" 1.0 1.0 1.0 0 0.0 29.0 (Oxygen 1.0) [moon]
-}

{-
solStar :: Star
solStar = Star "Sol" 5800 [earth]

solSS :: StarSystem
solSS = StarSystem "Sol" (0, 0, 0) [solStar]

milkyWay :: Galaxy
milkyWay = Galaxy [solSS]
-}

randomRM :: (RandomGen g, Random a) => (a, a) -> State g a
randomRM v = do
  g <- get
  (x, g') <- return $ randomR v g
  put g'
  return x

minStarTemperature = 3000
maxStarTemperature = 25000

createAtmosphere :: (RandomGen g) => Flt -> State g Atmosphere
createAtmosphere mass = do
  if mass < 0.01 then return NoAtmosphere
   else if mass > 25.0 then return GasGiant
    else do
      r <- randomRM (1, 4)
      return $! toEnum r

createPlanet :: (RandomGen g) => String -> Flt -> State g Planet
createPlanet name orbitradius = do
  orbit <- createOrbit orbitradius
  mass <- randomRM (0.01, 350) -- TODO: normal distribution
  atmosphere <- createAtmosphere mass
  numsatellites <- if mass < 300.0 then return 0 else randomRM (0, min 20 (floor (sqrt mass)))
  satelliteorbitradiuses <- sort `fmap` replicateM numsatellites (randomRM (0.002 * mass, 0.01 * mass))
  satellites <- zipWithM createPlanet (bodyNames name) satelliteorbitradiuses
  return $! Planet name orbit (BodyPhysics mass) atmosphere satellites

bodyNames :: String -> [String]
bodyNames = namesFromBasenameNum

createStar :: (RandomGen g) => String -> Flt -> Orbit -> State g Star
createStar name maxplanetorbitradius orbit = do
  t <- randomRM (minStarTemperature, maxStarTemperature) -- TODO: normal distribution
  numplanets <- randomRM (0, min 10 (floor (sqrt maxplanetorbitradius)))
  let planetnames = bodyNames name
  planetorbitradiuses <- sort `fmap` replicateM numplanets (randomRM (0.1, maxplanetorbitradius))
  -- TODO: make sure orbits aren't too close to each other
  planets <- zipWithM createPlanet planetnames planetorbitradiuses
  return $! Star name t orbit (filter (\p -> planetTemperature' t p > 30) planets)

namesFromBasenameCap :: String -> [String]
namesFromBasenameCap n = zipWith (++) (repeat (n ++ " ")) (map (:[]) ['A' .. 'Z'])

namesFromBasenameMin :: String -> [String]
namesFromBasenameMin n = zipWith (++) (repeat (n ++ " ")) (map (:[]) ['a' .. 'z'])

namesFromBasenameNum :: String -> [String]
namesFromBasenameNum n = zipWith (++) (repeat (n ++ " ")) (map show [1..])

createOrbit :: (RandomGen g) => Flt -> State g Orbit
createOrbit oradius = return $! Orbit oradius 0 0 1 0

-- distances [2, 3, 68, 70, 300, 4000] = [1, 1, 2, 2, 230, 3700]
distances :: (Num a, Ord a) => [a] -> [a]
distances []         = []
distances [a]        = [0]
distances [a, b]     = [b - a, b - a]
distances (a:b:c:ds) = b - a : go a b c ds
  where go a b c [] = min (b - a) (c - b) : c - b : []
        go a b c ds = min (b - a) (c - b) : go b c (head ds) (tail ds)

createStars :: (RandomGen g) => String -> Int -> State g [Star]
createStars basename numstars = do
  let names = if numstars == 1 then [basename] else namesFromBasenameCap basename
  orbitradiuses <- sort `fmap` replicateM numstars (randomRM (0.1, 12000)) -- TODO: normal distribution
  orbits <- mapM createOrbit orbitradiuses
  let dists = map (/4) (distances orbitradiuses)
  stars <- zipWith3M createStar names dists orbits
  return $! stars

zipWith3M :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M f [] _ _ = return []
zipWith3M f _ [] _ = return []
zipWith3M f _ _ [] = return []
zipWith3M f (a:as) (b:bs) (c:cs) = do
  n <- f a b c
  rest <- zipWith3M f as bs cs
  return (n:rest)

createStarSystem :: (RandomGen g) => String -> Vector3 -> State g StarSystem
createStarSystem ssname sspos = do
  n <- randomRM (1, 6 :: Int)
  stars <- createStars ssname n
  return $! StarSystem ssname sspos stars

create2DPoint :: (RandomGen g) => (Flt, Flt) -> State g Vector3
create2DPoint (minc, maxc) = do
  x <- randomRM (minc, maxc)
  y <- randomRM (minc, maxc)
  return (x, y, 0)

create3DPoint :: (RandomGen g) => (Flt, Flt) -> State g Vector3
create3DPoint (minc, maxc) = do
  x <- randomRM (minc, maxc)
  y <- randomRM (minc, maxc)
  z <- randomRM (minc, maxc)
  return (x, y, z)

ssSpacingCoefficient :: Float
ssSpacingCoefficient = 5

createGalaxy :: (RandomGen g) => String -> [String] -> State g Galaxy
createGalaxy galname ssnames = do
  let numss = length ssnames
  let dim = sqrt (fromIntegral numss) * ssSpacingCoefficient -- TODO: when 3d galaxy, use cbrt
  points <- replicateM numss (create3DPoint (-dim, dim))
  sss <- zipWithM createStarSystem ssnames points
  return $! Galaxy galname sss

nearsystems :: [String]
nearsystems = ["Alpha Centauri",
               "Barnard's Star",
               "Wolf 359",
               "Lalande 21185",
               "Sirius",
               "Lyuten 726-8",
               "Ross 154",
               "Ross 248",
               "Epsilon Eridani",
               "Lacaille 9352",
               "Ross 128",
               "EZ Aquarii",
               "Procyon",
               "61 Cygni",
               "Tau Ceti",
               "Fomalhaut"
              ]

testGalaxy :: Galaxy
testGalaxy = 
  let r = mkStdGen 20
  in evalState (createGalaxy "milky way" nearsystems) r

testStars :: [Star]
testStars = concatMap stars (starsystems testGalaxy)

testPlanets :: [Planet]
testPlanets = concatMap planets testStars

testSatellites :: [Planet]
testSatellites = concatMap satellites testPlanets

planetTemperatures :: Star -> [Temperature]
planetTemperatures s = map (planetTemperature s) (planets s)

