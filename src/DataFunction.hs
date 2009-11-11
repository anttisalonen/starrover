module DataFunction
where

import Data.Maybe
import qualified Data.Map as M

import Utils
import Math
import Galaxy

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
planetTemperature' t pl =
  planetTemperature''
    t
    (orbitradius (orbit pl))
    (planettype pl)

planetTemperatureByStar :: Star a -> Planet a -> Temperature
planetTemperatureByStar s p = planetTemperature' (temperature s) p

planetTemperatureByStarSystem :: (Eq a) => StarSystem a -> Planet a -> Temperature
planetTemperatureByStarSystem ss p = case starInSystemByPlanet p ss of
                                       Nothing -> 0
                                       Just s  -> planetTemperatureByStar s p

planetTemperature = planettemperature

starsInsidePlanetOrbit :: (Eq a) => StarSystem a -> Planet a -> [Star a]
starsInsidePlanetOrbit systems p = 
  let sts = (M.elems . stars) systems
  in dropWhile (planetAroundStar p) (reverse sts)

planetAroundStar :: (Eq a) => Planet a -> Star a -> Bool
planetAroundStar p s = p `elem` (M.elems (planets s))

starInSystemByPlanet :: (Eq a) => Planet a -> StarSystem a -> Maybe (Star a)
starInSystemByPlanet p ss = listToMaybe (filter (planetAroundStar p) ((M.elems . stars) ss))

sustainsLife :: Planet a -> Bool
sustainsLife p = 
  planettype p == RockyPlanet WaterWeatherSystem && 
  planetMass p > 0.01  && 
  planetMass p < 20.0  && 
  planetTemperature p < 320 && 
  planetTemperature p > 250

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

allPlanets :: Galaxy a -> [Planet a]
allPlanets g = concatMap (M.elems . planets) (allStars g)

allBodies :: Galaxy a -> [Planet a]
allBodies = concatMap bodiesInStarSystem . M.elems . starsystems

bodiesAroundPlanet = M.elems . satellites

bodiesAroundStar :: Star a -> [Planet a]
bodiesAroundStar s = let pls = (M.elems . planets) s in pls ++ (concatMap bodiesAroundPlanet pls)

bodiesInStarSystem :: StarSystem a -> [Planet a]
bodiesInStarSystem s = concatMap bodiesAroundStar ((M.elems . stars) s)

{-
planetTemperatures :: Star a -> StarSystem a -> [Temperature]
planetTemperatures s ss = map (planetTemperature ss) ((M.elems . planets) s)
-}

linkStarSystemToPlanet :: (Eq a) => StarSystem a -> Planet a -> Maybe (Star a)
linkStarSystemToPlanet ss p = 
  let l = filter (\s -> p `elem` (M.elems (planets s))) ((M.elems . stars) ss)
  in if null l then Nothing else Just $ head l


