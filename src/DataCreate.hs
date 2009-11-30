{-# LANGUAGE Rank2Types #-} 
module DataCreate
where

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
import ZipperGalaxyUtils

gasGiantByMass :: Flt -> PlanetType
gasGiantByMass mass | mass < 20.0  = SmallGasGiant
                    | mass < 100.0 = MediumGasGiant
                    | mass < 400.0 = LargeGasGiant
                    | otherwise    = VeryLargeGasGiant

createPlanetType :: Flt -> Temperature -> Flt -> Rnd PlanetType
createPlanetType mass startemp orbitradius = do
  if mass < 0.001 then return Planetoid
   else if mass < 0.01 then return NoAtmosphere
   else if mass > 15.0 then return (gasGiantByMass mass)
   else createRockyPlanetAtmosphere mass startemp orbitradius

createRockyPlanetAtmosphere :: Flt -> Temperature -> Flt -> Rnd PlanetType
createRockyPlanetAtmosphere mass startemp orbitradius = do
  weather <- randomRM (1, 100 :: Int)
  let atm1 = if weather < 10
               then RockyPlanet WaterWeatherSystem 
               else if weather < 30 then RockyPlanet MethaneWeatherSystem
               else if weather < 50 then RockyPlanet SulphurDioxide
               else if weather < 90 then RockyPlanet CarbonDioxide
               else RockyPlanet Nitrogen
  let ptemp = planetTemperature'' startemp orbitradius atm1
  return $! case atm1 of
    RockyPlanet WaterWeatherSystem   -> if ptemp < waterMaxTemperature && ptemp > waterMinTemperature then atm1 else RockyPlanet Nitrogen
    RockyPlanet MethaneWeatherSystem -> if ptemp < 120 && ptemp > 70  then atm1 else RockyPlanet Nitrogen
    _                                -> atm1

createSatellite :: Flt -> Flt -> (Planet () -> Rnd a) -> Temperature -> String -> Flt -> Rnd (Planet a)
createSatellite minmass maxmass genfunc startemp name orbitradius = do
  orbit <- createOrbit orbitradius
  mass <- randomRM (minmass, maxmass)
  atmosphere <- createPlanetType mass startemp orbitradius
  let ptemp = planetTemperature'' startemp orbitradius atmosphere
  let emptyplanet = Planet name orbit (BodyPhysics mass) atmosphere ptemp M.empty ()
  cont <- genfunc emptyplanet
  return $! Planet name orbit (BodyPhysics mass) atmosphere ptemp M.empty cont

createPlanet :: (Planet () -> Rnd a) -> Temperature -> String -> Flt -> Rnd (Planet a)
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
  let temp = planetTemperature'' startemp orbitradius atmosphere
  cont <- genfunc (Planet name orbit (BodyPhysics mass) atmosphere temp M.empty ())
  return $! Planet name orbit (BodyPhysics mass) atmosphere temp (namedsToMap satellites) cont

bodyNames :: String -> [String]
bodyNames = namesFromBasenameNum

starprobs, starbinaryprobs :: [(Flt, SpectralType)]
starprobs       = [(0.001, SpectralTypeB), (0.004, SpectralTypeA), (0.02, SpectralTypeF), (0.06, SpectralTypeG), (0.12, SpectralTypeK), (1, SpectralTypeM)]
starbinaryprobs = [(0.001, SpectralTypeB), (0.010, SpectralTypeA), (0.10, SpectralTypeF), (0.25, SpectralTypeG), (0.40, SpectralTypeK), (1, SpectralTypeM)]

getProbTableValue :: (Ord a) => a -> [(a, b)] -> b
getProbTableValue n [(_, v)] = v
getProbTableValue n ((k, v):xs) = if n <= k then v else getProbTableValue n xs

randomStarTemperature :: SpectralType -> Rnd Temperature
randomStarTemperature s = randomRM (specTempRange s)

createStar :: (Planet () -> Rnd a) -> String -> Flt -> Orbit -> Rnd (Star a)
createStar genfunc name maxplanetorbitradius orbit = do
  r <- randomRM (0, 1)
  let s = getProbTableValue r starprobs
  createStar' genfunc name maxplanetorbitradius orbit s

createStar' :: (Planet () -> Rnd a) -> String -> Flt -> Orbit -> SpectralType -> Rnd (Star a)
createStar' genfunc name maxplanetorbitradius orbit spectraltype = do
  t <- randomStarTemperature spectraltype
  numplanets <- randomRM (64, 128)
  let planetnames = bodyNames name
  planetorbitradiuses <- separate `fmap` sort `fmap` replicateM numplanets (randomRM (0.0001, min (fromIntegral t * 2 / 120) maxplanetorbitradius))
  planets <- zipWithM (createPlanet genfunc t) (repeat "") planetorbitradiuses
  let planets' = filter (\p -> planetTemperature' t p > 30 && planetTemperature' t p < t `div` 4) planets
  let changename n p = p{planetname = n}
  let planets'' = zipWith changename planetnames planets'
  return $! Star name t orbit (namedsToMap planets'')

namesFromBasenameCap :: String -> [String]
namesFromBasenameCap n = zipWith (++) (repeat (n ++ " ")) (map (:[]) ['A' .. 'Z'])

namesFromBasenameMin :: String -> [String]
namesFromBasenameMin n = zipWith (++) (repeat (n ++ " ")) (map (:[]) ['a' .. 'z'])

namesFromBasenameNum :: String -> [String]
namesFromBasenameNum n = zipWith (++) (repeat (n ++ " ")) (map show [1..])

createOrbit :: Flt -> Rnd Orbit
createOrbit oradius = return $! Orbit oradius 0 0 1 0

noOrbit :: Orbit
noOrbit = Orbit 0 0 0 1 0

createStars :: (Planet () -> Rnd a) -> String -> Int -> Rnd [Star a]
createStars genfunc basename 1 = do
  s <- createStar genfunc basename 3000 noOrbit
  return [s]
createStars genfunc basename numstars = do
  let names = namesFromBasenameCap basename
  orbitradiuses <- sort `fmap` replicateM numstars (randomRM (0.1, 12000))
  orbits <- mapM createOrbit orbitradiuses
  let dists = map (/4) (distances orbitradiuses)
  spectraltypes <- map (flip getProbTableValue starbinaryprobs) `fmap` replicateM numstars (randomRM (0, 1))
  stars <- zipWith4M (createStar' genfunc) names dists orbits (sort spectraltypes)
  return $! stars

createStarSystem :: (Planet () -> Rnd a) -> String -> Vector3 -> Rnd (StarSystem a)
createStarSystem genfunc ssname sspos = do
  -- http://www.cfa.harvard.edu/news/2006/pr200611.html 
  -- "Most milky way stars are single"
  singular <- chance 2 3 
  n <- if singular then return 1 else randomRM (2, 6 :: Int)
  stars <- (createStars genfunc) ssname n
  return $! StarSystem ssname sspos (namedsToMap stars)

create2DPoint :: (Flt, Flt) -> Rnd Vector3
create2DPoint (minc, maxc) = do
  x <- randomRM (minc, maxc)
  y <- randomRM (minc, maxc)
  return (x, y, 0)

create3DPoint :: (Flt, Flt) -> Rnd Vector3
create3DPoint (minc, maxc) = do
  x <- randomRM (minc, maxc)
  y <- randomRM (minc, maxc)
  z <- randomRM (minc, maxc)
  return (x, y, z)

ssSpacingCoefficient :: Float
ssSpacingCoefficient = 5

createGalaxy :: (Planet () -> Rnd a) -> String -> [String] -> Rnd (Galaxy a)
createGalaxy genfunc galname ssnames = do
  let numss = length ssnames
  let dim = sqrt (fromIntegral numss) * ssSpacingCoefficient -- TODO: when 3d galaxy, use cbrt
  points <- replicateM numss (create3DPoint (-dim, dim))
  sss <- zipWithM (createStarSystem genfunc) ssnames points
  return $! Galaxy galname (namedsToMap sss)

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
  in evalState (createGalaxy (createTerrain stdGoods) "milky way" (take numsys $ nearsystems ++ map show [1..numsys])) r

createTerrain :: [Good] -> Planet () -> Rnd Terrain
createTerrain gs p = 
  case planettype p of
    Planetoid         -> createRockyTerrain gs p
    NoAtmosphere      -> createRockyTerrain gs p
    RockyPlanet _     -> createRockyTerrain gs p
    SmallGasGiant     -> return (Terrain [] [])
    MediumGasGiant    -> return (Terrain [] [])
    LargeGasGiant     -> return (Terrain [] [])
    VeryLargeGasGiant -> return (Terrain [] [])

createRockyTerrain :: [Good] -> Planet () -> Rnd Terrain
createRockyTerrain gs p = do
  massmult <- randomRM (0, 1000 * planetMass p)
  gs' <- mapMaybeM (createNaturalGood massmult (planettype p)) gs
  return (Terrain gs' [])

createNaturalGood :: Flt -> PlanetType -> Good -> Rnd (Maybe (Resource, ResourceUnit))
createNaturalGood massmult pt g = 
  case natural g of
    Nothing                       -> return Nothing
    Just (Natural atms _ initial) -> do
      let atmNeeded = not $ null atms
      let invAtm = if not atmNeeded 
                     then False
                     else case pt of
                       RockyPlanet a -> a `notElem` atms
                       _             -> True
      if invAtm 
        then return Nothing
        else do
          mult <- randomRM (0, initial)
          let v = floor $ mult * massmult
          return $ Just ((g, v), v)


