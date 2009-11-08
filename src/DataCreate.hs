module DataCreate
where

import Data.Char (chr)
import Data.List (sort)
import Control.Monad.State
import System.Random
import qualified Data.Map as M

import Galaxy
import Civilization
import DataFunction
import Statistics
import Math
import Utils
import ZipperGalaxy
import ZipperGalaxyUtils

gasGiantByMass :: Flt -> PlanetType
gasGiantByMass mass | mass < 20.0  = SmallGasGiant
                    | mass < 100.0 = MediumGasGiant
                    | mass < 400.0 = LargeGasGiant
                    | otherwise    = VeryLargeGasGiant

createPlanetType :: (RandomGen g) => Flt -> Temperature -> Flt -> State g PlanetType
createPlanetType mass startemp orbitradius = do
  if mass < 0.001 then return Planetoid
   else if mass < 0.01 then return NoAtmosphere
   else if mass > 15.0 then return (gasGiantByMass mass)
   else createRockyPlanetAtmosphere mass startemp orbitradius

createRockyPlanetAtmosphere :: (RandomGen g) => Flt -> Temperature -> Flt -> State g PlanetType
createRockyPlanetAtmosphere mass startemp orbitradius = do
  weather <- randomRM (1, 100 :: Int)
  let atm1 = if weather < 5
               then RockyPlanet WaterWeatherSystem 
               else if weather < 10 then RockyPlanet MethaneWeatherSystem
               else if weather < 20 then RockyPlanet SulphurDioxide
               else if weather < 60 then RockyPlanet CarbonDioxide
               else RockyPlanet Nitrogen
  return $!
    case atm1 of
      RockyPlanet WaterWeatherSystem   -> if (planetTemperature'' startemp orbitradius atm1) > 330 then RockyPlanet Nitrogen else atm1
      RockyPlanet MethaneWeatherSystem -> if (planetTemperature'' startemp orbitradius atm1) > 600 then RockyPlanet Nitrogen else atm1
      _                                -> atm1

createSatellite :: (RandomGen g) => Flt -> Flt -> (Planet () -> State g a) -> Temperature -> String -> Flt -> State g (Planet a)
createSatellite minmass maxmass genfunc startemp name orbitradius = do
  orbit <- createOrbit orbitradius
  mass <- randomRM (minmass, maxmass)
  atmosphere <- createPlanetType mass startemp orbitradius
  let emptyplanet = Planet name orbit (BodyPhysics mass) atmosphere M.empty ()
  cont <- genfunc emptyplanet
  return $! Planet name orbit (BodyPhysics mass) atmosphere M.empty cont

createPlanet :: (RandomGen g) => (Planet () -> State g a) -> Temperature -> String -> Flt -> State g (Planet a)
createPlanet genfunc startemp name orbitradius = do
  orbit <- createOrbit orbitradius
  gentype <- randomRM (1, 4 :: Int)
  mass <- case gentype of
            1 -> randomRM (50, 400)
            2 -> randomRM (20, 50)
            3 -> randomRM (0.0001, 0.1)
            _ -> randomRM (0.1, 20)
  atmosphere <- createPlanetType mass startemp orbitradius
  numsatellites <- if mass < 1.0 then return 0 else randomRM (0 :: Int, min 16 (floor (sqrt mass)))
  satellites <- zipWithM (createSatellite (0.00001 * mass) (0.01 * mass) genfunc startemp) (bodyNames name) (replicate numsatellites orbitradius)
  cont <- genfunc (Planet name orbit (BodyPhysics mass) atmosphere M.empty ())
  return $! Planet name orbit (BodyPhysics mass) atmosphere (stdMap satellites) cont

stdMap :: [a] -> M.Map Int a
stdMap xs = M.fromList (zip [1..] xs)

bodyNames :: String -> [String]
bodyNames = namesFromBasenameNum

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
  numplanets <- randomRM (64, 128)
  let planetnames = bodyNames name
  planetorbitradiuses <- separate `fmap` sort `fmap` replicateM numplanets (randomRM (0.01, min (fromIntegral t * 2 / 100) maxplanetorbitradius))
  planets <- zipWithM (createPlanet genfunc t) planetnames planetorbitradiuses
  return $! Star name t orbit (stdMap (filter (\p -> planetTemperature' t p > 30 && planetTemperature' t p < t `div` 4) planets))

namesFromBasenameCap :: String -> [String]
namesFromBasenameCap n = zipWith (++) (repeat (n ++ " ")) (map (:[]) ['A' .. 'Z'])

namesFromBasenameMin :: String -> [String]
namesFromBasenameMin n = zipWith (++) (repeat (n ++ " ")) (map (:[]) ['a' .. 'z'])

namesFromBasenameNum :: String -> [String]
namesFromBasenameNum n = zipWith (++) (repeat (n ++ " ")) (map show [1..])

createOrbit :: (RandomGen g) => Flt -> State g Orbit
createOrbit oradius = return $! Orbit oradius 0 0 1 0

noOrbit :: Orbit
noOrbit = Orbit 0 0 0 1 0

createStars :: (RandomGen g) => (Planet () -> State g a) -> String -> Int -> State g [Star a]
createStars genfunc basename 1 = do
  s <- createStar genfunc basename 3000 noOrbit
  return [s]
createStars genfunc basename numstars = do
  let names = namesFromBasenameCap basename
  orbitradiuses <- sort `fmap` replicateM numstars (randomRM (0.1, 12000))
  orbits <- mapM createOrbit orbitradiuses
  let dists = map (/4) (distances orbitradiuses)
  stars <- zipWith3M (createStar genfunc) names dists orbits
  return $! stars

createStarSystem :: (RandomGen g) => (Planet () -> State g a) -> String -> Vector3 -> State g (StarSystem a)
createStarSystem genfunc ssname sspos = do
  singular <- randomRM (False, True)
  n <- if singular then return 1 else randomRM (2, 6 :: Int)
  stars <- (createStars genfunc) ssname n
  return $! StarSystem ssname sspos (stdMap stars)

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
  return $! Galaxy galname (stdMap sss)

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
  in evalState (createGalaxy (\_ -> return (Terrain [])) "milky way" (take numsys $ nearsystems ++ map show [1..numsys])) r

createLife :: (RandomGen g) => Galaxy Terrain -> String -> State g (Maybe Civilization)
createLife g cname = do
  let ps = filter (uncurry sustainsLife) (planetsWithStarSystemInGalaxy g)
  if null ps 
    then return Nothing
    else do
      (ss, p) <- choose ps
      let mzip = findZipperGalaxyToPlanet p g
      case mzip of
        Nothing -> return Nothing
        Just s  -> return $! Just $ Civilization cname ([Settlement s])

testCiv :: Maybe Civilization
testCiv = 
  let r = mkStdGen 20
  in evalState (createLife (testRandomGalaxy 21 256) "humans") r

