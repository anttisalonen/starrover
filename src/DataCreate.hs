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
moon = Planet a "Moon" 0.015 0.8 1.0 0.0 0.0 10.0 NoAtmosphere []

earth :: Planet
earth = Planet a "Earth" 1.0 1.0 1.0 0 0.0 29.0 (Oxygen 1.0) [moon]
-}

{-
solStar a :: Star
solStar a = Star a "Sol" 5800 [earth]

solSS :: StarSystem
solSS = StarSystem a "Sol" (0, 0, 0) [solStar]

milkyWay :: Galaxy
milkyWay = Galaxy a [solSS]
-}

createAtmosphere :: (RandomGen g) => Flt -> Temperature -> Flt -> State g Atmosphere
createAtmosphere mass startemp orbitradius = do
  if mass < 0.01 then return NoAtmosphere
   else if mass > 15.0 then return GasGiant
    else createRockyPlanetAtmosphere startemp orbitradius

createRockyPlanetAtmosphere :: (RandomGen g) => Temperature -> Flt -> State g Atmosphere
createRockyPlanetAtmosphere startemp orbitradius = do
  life <- (== 1) `fmap` randomRM (1, 100 :: Int)
  r <- randomRM (1, 4)
  return $! toEnum r

createSatellite :: (RandomGen g) => Flt -> Flt -> (Planet () -> State g a) -> Temperature -> String -> Flt -> State g (Planet a)
createSatellite minmass maxmass genfunc startemp name orbitradius = do
  orbit <- createOrbit orbitradius
  mass <- randomRM (minmass, maxmass)
  atmosphere <- createAtmosphere mass startemp orbitradius
  let emptyplanet = Planet name orbit (BodyPhysics mass) atmosphere [] ()
  cont <- genfunc emptyplanet
  return $! Planet name orbit (BodyPhysics mass) atmosphere [] cont

createPlanet :: (RandomGen g) => (Planet () -> State g a) -> Temperature -> String -> Flt -> State g (Planet a)
createPlanet genfunc startemp name orbitradius = do
  orbit <- createOrbit orbitradius
  -- mass <- (^3) `fmap` randomRM (0.1, 7.5)
  mass <- (**5.4) `fmap` randomRM (0.1, 3)
  -- mass <- (^2) `fmap` randomRM (0.1, 20)
  atmosphere <- createAtmosphere mass startemp orbitradius
  numsatellites <- if mass < 1.0 then return 0 else randomRM (0, min 10 (floor (sqrt mass)))
  satelliteorbitradiuses <- sort `fmap` replicateM numsatellites (randomRM (0.002 * mass, 0.01 * mass))
  satellites <- zipWithM (createSatellite (0.00001 * mass) (0.01 * mass) genfunc startemp) (bodyNames name) satelliteorbitradiuses
  cont <- genfunc (Planet name orbit (BodyPhysics mass) atmosphere [] ())
  return $! Planet name orbit (BodyPhysics mass) atmosphere satellites cont

bodyNames :: String -> [String]
bodyNames = namesFromBasenameNum

minStarTemperature = 3000
maxStarTemperature = 25000

starprobs :: [(Flt, SpectralType)]
starprobs = [(0.001, SpectralTypeB), (0.007, SpectralTypeA), (0.04, SpectralTypeF), (0.11, SpectralTypeG), (0.23, SpectralTypeK), (1, SpectralTypeM)]

getProbTableValue :: (Ord a) => a -> [(a, b)] -> b
getProbTableValue n [(_, v)] = v
getProbTableValue n ((k, v):xs) = if n <= k then v else getProbTableValue n xs

randomStarTemperature :: (RandomGen g) => SpectralType -> State g Temperature
randomStarTemperature s = randomRM (specTempRange s)

createStar :: (RandomGen g) => (Planet () -> State g a) -> String -> Flt -> Orbit -> State g (Star a)
createStar genfunc name maxplanetorbitradius orbit = do
  r <- randomRM (0, 1)
  let s = getProbTableValue r starprobs
  t <- randomStarTemperature s
  numplanets <- randomRM (0, min 16 (floor (maxplanetorbitradius / 2)))
  let planetnames = bodyNames name
  planetorbitradiuses <- sort `fmap` replicateM numplanets (randomRM (0.1, maxplanetorbitradius))
  -- TODO: make sure orbits aren't too close to each other
  planets <- zipWithM (createPlanet genfunc t) planetnames planetorbitradiuses
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

createStars :: (RandomGen g) => (Planet () -> State g a) -> String -> Int -> State g [Star a]
createStars genfunc basename numstars = do
  let names = if numstars == 1 then [basename] else namesFromBasenameCap basename
  orbitradiuses <- sort `fmap` replicateM numstars (randomRM (0.1, 12000)) -- TODO: normal distribution
  orbits <- mapM createOrbit orbitradiuses
  let dists = map (/4) (distances orbitradiuses)
  stars <- zipWith3M (createStar genfunc) names dists orbits
  return $! stars

zipWith3M :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M f [] _ _ = return []
zipWith3M f _ [] _ = return []
zipWith3M f _ _ [] = return []
zipWith3M f (a:as) (b:bs) (c:cs) = do
  n <- f a b c
  rest <- zipWith3M f as bs cs
  return (n:rest)

createStarSystem :: (RandomGen g) => (Planet () -> State g a) -> String -> Vector3 -> State g (StarSystem a)
createStarSystem genfunc ssname sspos = do
  n <- randomRM (1, 6 :: Int)
  stars <- (createStars genfunc) ssname n
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

createGalaxy :: (RandomGen g) => (Planet () -> State g a) -> String -> [String] -> State g (Galaxy a)
createGalaxy genfunc galname ssnames = do
  let numss = length ssnames
  let dim = sqrt (fromIntegral numss) * ssSpacingCoefficient -- TODO: when 3d galaxy, use cbrt
  points <- replicateM numss (create3DPoint (-dim, dim))
  sss <- zipWithM (createStarSystem genfunc) ssnames points
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
               "Fomalhaut",
               "Struve 2398",
               "Groombridge 34",
               "Epsilon Indi",
               "DX Cancri",
               "GJ 1061",
               "YZ Ceti", 
               "Lyuten's Star",
               "Kapteyn's Star"
              ]

testGalaxy :: Galaxy Terrain
testGalaxy = testRandomGalaxy 20 16

testRandomGalaxy :: Int -> Int -> Galaxy Terrain
testRandomGalaxy v numsys =
  let r = mkStdGen v
  in evalState (createGalaxy (\_ -> return (Terrain [])) "milky way" (map show [1..numsys])) r

testStars :: [Star Terrain]
testStars = concatMap stars (starsystems testGalaxy)

testPlanets :: [Planet Terrain]
testPlanets = concatMap planets testStars

testSatellites :: [Planet Terrain]
testSatellites = concatMap satellites testPlanets

allPlanets :: Galaxy a -> [Planet a]
allPlanets g = concatMap planets (allStars g)

allSatellites :: Galaxy a -> [Planet a]
allSatellites g = concatMap satellites (allPlanets g)

allPlanetsAndSatellites :: Galaxy a -> [Planet a]
allPlanetsAndSatellites g = allPlanets g ++ allSatellites g

choose :: (RandomGen g) => [a] -> State g a
choose l = do
  let n = length l
  i <- randomRM (0, n - 1)
  return (l !! i)

createLife :: (RandomGen g) => Galaxy a -> String -> State g (Maybe (Civilization a))
createLife g cname = do
  let ps = filter (uncurry sustainsLife) (starPlanetPairs g)
  if null ps 
    then return Nothing
    else do
      (s, p) <- choose ps
      return $! Just $ Civilization cname ([Settlement p s])

testCiv :: Maybe (Civilization Terrain)
testCiv = 
  let r = mkStdGen 20
  in evalState (createLife testGalaxy "humans") r

