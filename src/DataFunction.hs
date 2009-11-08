module DataFunction
where

import Control.Monad.State
import System.Random
import Data.Maybe
import qualified Data.Map as M

import DataTypes

mass :: Flt -> Flt -> Flt
mass density volume = density / volume

density :: Flt -> Flt -> Flt
density mass volume = mass / volume

specTempRangeB = (10000, 25000)
specTempRangeA = (7500, 10000)
specTempRangeF = (6000, 7500)
specTempRangeG = (5200, 6000)
specTempRangeK = (3700, 5200)
specTempRangeM = (3000, 3700)

specTempRangeTable :: [(SpectralType, (Temperature, Temperature))]
specTempRangeTable = zip allEnums specTempRanges

specTempRanges = [specTempRangeB, specTempRangeA, specTempRangeF, specTempRangeG, specTempRangeK, specTempRangeM]

specTempRange :: SpectralType -> (Temperature, Temperature)
specTempRange s = fromJust $ lookup s specTempRangeTable

allEnums :: (Enum a, Bounded a) => [a]
allEnums = enumFrom minBound

specMinTemps = map fst specTempRanges

tempTable = zip allEnums specMinTemps

temperatureToSpectralType :: Temperature -> SpectralType
temperatureToSpectralType t = (fst . head . filter (\(st, t') -> t >= t')) tempTable

spectralType :: Star a -> SpectralType
spectralType s = temperatureToSpectralType (temperature s)

kelvinToCelsius :: Temperature -> Temperature
kelvinToCelsius = subtract 273

planetTemperature'' :: Temperature -> Flt -> PlanetType -> Temperature
planetTemperature'' t d planettype = floor $ 300 * (((fromIntegral t) / 6000) ^ 2) * (d ** (-0.7)) * acoeff
   where acoeff = case planettype of
           Planetoid         -> 0.8
           NoAtmosphere      -> 0.8
           RockyPlanet a     -> case a of
                                  MethaneWeatherSystem -> 1.5
                                  WaterWeatherSystem   -> 1.0
                                  Nitrogen             -> 0.9
                                  CarbonDioxide        -> 0.95
                                  SulphurDioxide       -> 1.1
           SmallGasGiant     -> 1.0
           MediumGasGiant    -> 1.0
           LargeGasGiant     -> 1.0
           VeryLargeGasGiant -> 1.0

planetTemperature' :: Temperature -> Planet a -> Temperature
planetTemperature' t planet =
  planetTemperature''
    t
    (orbitradius (orbit planet)) 
    (planettype planet)

planetTemperature :: StarSystem a -> Planet a -> Temperature
planetTemperature ss p = let ts = map temperature ((M.elems . stars) ss) in sum $ map (flip planetTemperature' p) ts

round100 :: Int -> Int
round100 i = i `div` 100 * 100

sustainsLife :: StarSystem a -> Planet a -> Bool
sustainsLife ss p = 
  planettype p == RockyPlanet WaterWeatherSystem && 
  planetMass p > 0.01  && 
  planetMass p < 20.0  && 
  planetTemperature ss p < 320 && 
  planetTemperature ss p > 250

planetMass :: Planet a -> Flt
planetMass = bodymass . physics

starPlanetPairs :: Galaxy a -> [(Star a, Planet a)]
starPlanetPairs g = unroll (zip (allStars g) (map (M.elems . planets) (allStars g)))

planetStars :: Galaxy a -> [([Star a], Planet a)]
planetStars g = 
  let ss = (M.elems . starsystems) g
      x  = concatMap planetsInStarSystem ss
      f = map (\(ss, p) -> ((M.elems . stars) ss, p))
  in f x

planetsWithStarSystemInGalaxy :: Galaxy a -> [(StarSystem a, Planet a)]
planetsWithStarSystemInGalaxy g = concatMap planetsInStarSystem ((M.elems . starsystems) g)

planetsInStarSystem :: StarSystem a -> [(StarSystem a, Planet a)]
planetsInStarSystem ss = zip (repeat ss) (((concatMap M.elems . map planets) . M.elems . stars) ss)

allStars :: Galaxy a -> [Star a]
allStars g = concatMap (M.elems . stars) ((M.elems . starsystems) g)

-- unroll [(1, [1,2,3]), (2, [3,4,5])] == [(1,1),(1,2),(1,3),(2,3),(2,4),(2,5)]
unroll :: [(a, [b])] -> [(a, b)]
unroll []           = []
unroll ((a, bs):xs) = (zip (repeat a) bs) ++ (unroll xs)

planetTemperatures :: Star a -> StarSystem a -> [Temperature]
planetTemperatures s ss = map (planetTemperature ss) ((M.elems . planets) s)

linkStarSystemToPlanet :: (Eq a) => StarSystem a -> Planet a -> Maybe (Star a)
linkStarSystemToPlanet ss p = 
  let l = filter (\s -> p `elem` (M.elems (planets s))) ((M.elems . stars) ss)
  in if null l then Nothing else Just $ head l

firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe f l = listToMaybe $ mapMaybe f l

findZipperGalaxyToPlanet :: (Eq a) => Planet a -> Galaxy a -> Maybe (GalaxyZipper a)
findZipperGalaxyToPlanet p g = firstMaybe (findZipperStarSystemToPlanet p g) (M.elems (starsystems g))

findZipperStarSystemToPlanet :: (Eq a) => Planet a -> Galaxy a -> StarSystem a -> Maybe (GalaxyZipper a)
findZipperStarSystemToPlanet p g ss = firstMaybe (findZipperStarToPlanet p g ss) (M.elems (stars ss))

findZipperStarToPlanet :: (Eq a) => Planet a -> Galaxy a -> StarSystem a -> Star a -> Maybe (GalaxyZipper a)
findZipperStarToPlanet p g ss s = 
  let pls = (M.elems . planets) s
  in if p `elem` pls
       then Just (g, Just (ss, Just (s, [p])))
       else firstMaybe (findZipperPlanetToPlanet p g ss s) pls

findZipperPlanetToPlanet :: (Eq a) => Planet a -> Galaxy a -> StarSystem a -> Star a -> Planet a -> Maybe (GalaxyZipper a)
findZipperPlanetToPlanet p g ss s p' = 
  if p `elem` (M.elems . satellites) p'
    then Just (g, Just (ss, Just (s, [p, p'])))
    else Nothing

randomRM :: (RandomGen g, Random a) => (a, a) -> State g a
randomRM v = do
  g <- get
  (x, g') <- return $ randomR v g
  put g'
  return x

stdNormal :: (RandomGen g, Random a, Ord a, Floating a) => State g a
stdNormal = do
  u1 <- randomRM (-1, 1)
  u2 <- randomRM (-1, 1)
  let m = stdNormalMarsaglia u1 u2
  case m of
    Nothing      -> stdNormal
    Just (z1, _) -> return z1

stdNormalMarsaglia :: (Ord a, Floating a) => a -> a -> Maybe (a, a)
stdNormalMarsaglia y1 y2 = 
  if q > 1 then Nothing else Just (z1, z2)
  where z1 = y1 * p
        z2 = y2 * p
        q = y1 * y1 + y2 * y2
        p = sqrt ((-2) * log q / q)

normal :: (RandomGen g, Random a, Ord a, Floating a) => a -> a -> State g a
normal mu sigma = do
  n <- stdNormal
  return $ mu + n * sigma

normalR :: (RandomGen g, Random a, Ord a, Floating a) => (a, a) -> a -> a -> State g a
normalR (mn, mx) mu sigma = do
  n <- normal mu sigma
  if n < mn 
    then return mn 
    else if n > mx
           then return mx else return n

normalIO :: (Random a, Ord a, Floating a) => a -> a -> IO a
normalIO mu sigma = newStdGen >>= return . evalState (normal mu sigma)

normalRIO :: (Random a, Ord a, Floating a) => (a, a) -> a -> a -> IO a
normalRIO limits mu sigma = newStdGen >>= return . evalState (normalR limits mu sigma)

average :: (Fractional a) => [a] -> a
average l = go 0 0 l
  where go acc len []     = acc / len
        go acc len (x:xs) = go (acc + x) (len + 1) xs

averageInt :: [Int] -> Int
averageInt l = go 0 0 l
  where go acc len []     = acc `div` len
        go acc len (x:xs) = go (acc + x) (len + 1) xs

median :: (Num a) => [a] -> a
median [] = 0
median (x:xs) = go 0 x xs
   where go _ x []     = x
         go 0 x (n:ns) = go 1 x ns
         go 1 x (n:ns) = go 1 n ns

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx v = if v < mn then mn else if v > mx then mx else v

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (h:_) = Just h


