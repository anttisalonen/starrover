module GalaxyStats
where

import qualified Data.Map as M
import Text.Printf

import Galaxy
import DataFunction
import Statistics
import Named
import Utils

numStarSystemsInGalaxy :: Galaxy a -> Int
numStarSystemsInGalaxy = M.size . starsystems

numStarsInSystem :: StarSystem a -> Int
numStarsInSystem = M.size . stars

numStarsInGalaxy :: Galaxy a -> Int
numStarsInGalaxy g = sum $ map numStarsInSystem ((M.elems . starsystems) g)

starsPerStarSystemInGalaxy :: Galaxy a -> Float
starsPerStarSystemInGalaxy g = fromIntegral (numStarsInGalaxy g) / fromIntegral (numStarSystemsInGalaxy g)

bodiesPerStarsInGalaxy g = fromIntegral (numBodiesInGalaxy g) / fromIntegral (numStarsInGalaxy g)
planetsPerStarsInGalaxy g = fromIntegral (numPlanetsInGalaxy g) / fromIntegral (numStarsInGalaxy g)
bodiesPerStarsystemsInGalaxy g = fromIntegral (numBodiesInGalaxy g) / fromIntegral (numStarSystemsInGalaxy g)
planetsPerStarsystemsInGalaxy g = fromIntegral (numPlanetsInGalaxy g) / fromIntegral (numStarSystemsInGalaxy g)
moonsPerBodiesInGalaxy g = fromIntegral (numMoonsInGalaxy g) / fromIntegral (numBodiesInGalaxy g - numMoonsInGalaxy g)

numBodiesOrbitingStar :: Star a -> Int
numBodiesOrbitingStar s = 
  let ps = (M.elems . planets) s
  in length ps + sum (map numSatellites ps)

numSatellites :: Planet a -> Int
numSatellites = M.size . satellites

numBodiesInStarSystem :: StarSystem a -> Int
numBodiesInStarSystem = sum . map numBodiesOrbitingStar . M.elems . stars

numBodiesInGalaxy :: Galaxy a -> Int
numBodiesInGalaxy = sum . map numBodiesInStarSystem . M.elems . starsystems

starsInGalaxy :: Galaxy a -> [Star a]
starsInGalaxy = concatMap M.elems . map stars . M.elems . starsystems

moonsInGalaxy :: Galaxy a -> [Planet a]
moonsInGalaxy = concatMap M.elems . map satellites . allBodies

numMoonsInGalaxy :: Galaxy a -> Int
numMoonsInGalaxy = length . moonsInGalaxy

numStarsByType t g = (filter (\s -> spectralType s == t)) (starsInGalaxy g)

numBStarsInGalaxy = length . numStarsByType SpectralTypeB
numAStarsInGalaxy = length . numStarsByType SpectralTypeA
numFStarsInGalaxy = length . numStarsByType SpectralTypeF
numGStarsInGalaxy = length . numStarsByType SpectralTypeG
numKStarsInGalaxy = length . numStarsByType SpectralTypeK
numMStarsInGalaxy = length . numStarsByType SpectralTypeM

numBodiesByPlanetType a g = length $ (filter (\p -> planettype p == a)) (allBodies g)

numRockyBodiesByAtmosphere :: Atmosphere -> Galaxy a -> Int
numRockyBodiesByAtmosphere a g = length $ filter (\p -> planettype p == RockyPlanet a) (allBodies g)

isGasGiant p = case planettype p of
                 SmallGasGiant     -> True
                 MediumGasGiant    -> True
                 LargeGasGiant     -> True
                 VeryLargeGasGiant -> True
                 _                 -> False

numGasGiantsInGalaxy g = length $ (filter isGasGiant) (allBodies g)

numWaterWeatherSystemBodiesInGalaxy :: Galaxy a -> Int
numWaterWeatherSystemBodiesInGalaxy = numRockyBodiesByAtmosphere WaterWeatherSystem
numNoAtmosphereBodiesInGalaxy g = numBodiesByPlanetType NoAtmosphere g + numBodiesByPlanetType Planetoid g
numHabitableBodiesInGalaxy :: Galaxy a -> Int
numHabitableBodiesInGalaxy g = length $ filter sustainsLife (allBodies g)

showPerc :: Int -> Int -> String
showPerc a b = printf " (%.2f%%)" (100.0 * (fromIntegral a / fromIntegral b) :: Float)

showstars :: String -> Int -> Int -> String
showstars c st tot = c ++ show st ++ (showPerc st tot) ++ "\n"

planetMassesInGalaxy g = map planetMass (allBodies g)

minBodyMassInGalaxy = minimum . planetMassesInGalaxy

maxBodyMassInGalaxy = maximum . planetMassesInGalaxy

medBodyMassInGalaxy = median . planetMassesInGalaxy

avgBodyMassInGalaxy = average . planetMassesInGalaxy

planetTemperaturesInGalaxy g = map planetTemperature (allPlanets g)

minBodyTemperatureInGalaxy = kelvinToCelsius . minimum . planetTemperaturesInGalaxy

maxBodyTemperatureInGalaxy = kelvinToCelsius . maximum . planetTemperaturesInGalaxy

avgBodyTemperatureInGalaxy = kelvinToCelsius . averageInt . planetTemperaturesInGalaxy

medBodyTemperatureInGalaxy = kelvinToCelsius . median . planetTemperaturesInGalaxy

numPlanetsInGalaxy g = numBodiesInGalaxy g - numMoonsInGalaxy g

galaxyStats :: Galaxy a -> String
galaxyStats g = 
  let starstotal = numStarsInGalaxy g
      bodiestotal = numBodiesInGalaxy g
      bstars = numBStarsInGalaxy g
      astars = numAStarsInGalaxy g
      fstars = numFStarsInGalaxy g
      gstars = numGStarsInGalaxy g
      kstars = numKStarsInGalaxy g
      mstars = numMStarsInGalaxy g
      gasbodies = numGasGiantsInGalaxy g
      waterbodies = numWaterWeatherSystemBodiesInGalaxy g
      noatmbodies = numNoAtmosphereBodiesInGalaxy g
      habitablebodies = numHabitableBodiesInGalaxy g
  in
     "Galaxy name: " ++ name g ++ "\n" ++
     "Number of star systems: " ++ (show . numStarSystemsInGalaxy) g ++ "\n" ++
     "Number of stars: " ++ (show . numStarsInGalaxy) g ++ "\n" ++
     "Number of stars / star system: " ++ (show2f . starsPerStarSystemInGalaxy) g ++ "\n" ++
     showstars "Number of stars of spectral type B: " bstars starstotal ++
     showstars "Number of stars of spectral type A: " astars starstotal ++
     showstars "Number of stars of spectral type F: " fstars starstotal ++
     showstars "Number of stars of spectral type G: " gstars starstotal ++
     showstars "Number of stars of spectral type K: " kstars starstotal ++
     showstars "Number of stars of spectral type M: " mstars starstotal ++
     "Number of bodies: " ++ (show . numBodiesInGalaxy) g ++ "\n" ++
     "Number of planets: " ++ (show . numPlanetsInGalaxy) g ++ "\n" ++
     "Number of planets / starsystem: " ++ (show2f . planetsPerStarsystemsInGalaxy) g ++ "\n" ++
     "Number of planets / star: " ++ (show2f . planetsPerStarsInGalaxy) g ++ "\n" ++
     "Number of moons: " ++ (show . numMoonsInGalaxy) g ++ "\n" ++
     "Number of moons / planet: " ++ (show2f . moonsPerBodiesInGalaxy) g ++ "\n" ++
     "Minimum body mass (Earths): " ++ (show . minBodyMassInGalaxy) g ++ "\n" ++
     "Median body mass (Earths): " ++ (show . medBodyMassInGalaxy) g ++ "\n" ++
     "Average body mass (Earths): " ++ (show . avgBodyMassInGalaxy) g ++ "\n" ++
     "Maximum body mass (Earths): " ++ (show . maxBodyMassInGalaxy) g ++ "\n" ++
     "Minimum body temperature (Celsius): " ++ (show . minBodyTemperatureInGalaxy) g ++ "\n" ++
     "Median body temperature (Celsius): " ++ (show . medBodyTemperatureInGalaxy) g ++ "\n" ++
     "Average body temperature (Celsius): " ++ (show . avgBodyTemperatureInGalaxy) g ++ "\n" ++
     "Maximum body temperature (Celsius): " ++ (show . maxBodyTemperatureInGalaxy) g ++ "\n" ++
     showstars "Number of gas giants: " gasbodies bodiestotal ++
     showstars "Number of oxygen bodies: " waterbodies bodiestotal ++
     showstars "Number of bodies without atmosphere: " noatmbodies bodiestotal ++
     showstars "Number of habitable bodies: " habitablebodies bodiestotal

