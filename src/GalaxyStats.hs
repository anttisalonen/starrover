module GalaxyStats
where

import Data.Ratio
import Text.Printf
import DataTypes
import DataFunction

numStarSystemsInGalaxy :: Galaxy a -> Int
numStarSystemsInGalaxy = length . starsystems

numStarsInSystem :: StarSystem a -> Int
numStarsInSystem = length . stars

numStarsInGalaxy :: Galaxy a -> Int
numStarsInGalaxy g = sum $ map numStarsInSystem (starsystems g)

starsPerStarSystemInGalaxy :: Galaxy a -> Float
starsPerStarSystemInGalaxy g = fromIntegral (numStarsInGalaxy g) / fromIntegral (numStarSystemsInGalaxy g)

planetsPerStarsInGalaxy g = fromIntegral (numPlanetsInGalaxy g) / fromIntegral (numStarsInGalaxy g)

numBodiesOrbitingStar :: Star a -> Int
numBodiesOrbitingStar s = 
  let ps = planets s
  in length ps + sum (map numSatellites ps)

numSatellites :: Planet a -> Int
numSatellites = length . satellites

numBodiesInStarSystem :: StarSystem a -> Int
numBodiesInStarSystem = sum . map numBodiesOrbitingStar . stars

numPlanetsInGalaxy :: Galaxy a -> Int
numPlanetsInGalaxy = sum . map numBodiesInStarSystem . starsystems

starsInGalaxy = concatMap stars . starsystems

numStarsByType t g = (filter (\s -> spectralType s == t)) (starsInGalaxy g)

numBStarsInGalaxy = length . numStarsByType SpectralTypeB
numAStarsInGalaxy = length . numStarsByType SpectralTypeA
numFStarsInGalaxy = length . numStarsByType SpectralTypeF
numGStarsInGalaxy = length . numStarsByType SpectralTypeG
numKStarsInGalaxy = length . numStarsByType SpectralTypeK
numMStarsInGalaxy = length . numStarsByType SpectralTypeM

planetsAroundPlanet = satellites

planetsAroundStar :: Star a -> [Planet a]
planetsAroundStar s = let pls = planets s in pls ++ (concatMap planetsAroundPlanet pls)

planetsInStarSystem s = concatMap planetsAroundStar (stars s)

planetsInGalaxy = concatMap planetsInStarSystem . starsystems

numPlanetsByAtmosphere a g = length $ (filter (\p -> atmosphere p == a)) (planetsInGalaxy g)

numGasGiantsInGalaxy = numPlanetsByAtmosphere GasGiant
numOxygenPlanetsInGalaxy = numPlanetsByAtmosphere Oxygen
numNoAtmospherePlanetsInGalaxy = numPlanetsByAtmosphere NoAtmosphere
numHabitablePlanetsInGalaxy g = length $ filter (uncurry sustainsLife) (starPlanetPairs g)

showPerc :: Int -> Int -> String
showPerc a b = printf " (%.2f%%)" (100.0 * (fromIntegral a / fromIntegral b) :: Float)

showstars c st tot = c ++ show st ++ (showPerc st tot) ++ "\n"

planetMassesInGalaxy g = map planetMass (planetsInGalaxy g)

minPlanetMassInGalaxy = minimum . planetMassesInGalaxy

maxPlanetMassInGalaxy = maximum . planetMassesInGalaxy

medPlanetMassInGalaxy = median . planetMassesInGalaxy

avgPlanetMassInGalaxy = average . planetMassesInGalaxy

planetTemperaturesInGalaxy g = concatMap planetTemperatures (starsInGalaxy g)

minPlanetTemperatureInGalaxy = kelvinToCelsius . minimum . planetTemperaturesInGalaxy

maxPlanetTemperatureInGalaxy = kelvinToCelsius . maximum . planetTemperaturesInGalaxy

avgPlanetTemperatureInGalaxy = kelvinToCelsius . averageInt . planetTemperaturesInGalaxy

medPlanetTemperatureInGalaxy = kelvinToCelsius . median . planetTemperaturesInGalaxy

show2f :: Float -> String
show2f f = printf "%.2f" f

galaxyStats :: Galaxy a -> String
galaxyStats g = 
  let starstotal = numStarsInGalaxy g
      planetstotal = numPlanetsInGalaxy g
      bstars = numBStarsInGalaxy g
      astars = numAStarsInGalaxy g
      fstars = numFStarsInGalaxy g
      gstars = numGStarsInGalaxy g
      kstars = numKStarsInGalaxy g
      mstars = numMStarsInGalaxy g
      gasplanets = numGasGiantsInGalaxy g
      oxyplanets = numOxygenPlanetsInGalaxy g
      noatmplanets = numNoAtmospherePlanetsInGalaxy g
      habitableplanets = numHabitablePlanetsInGalaxy g
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
     "Number of planets: " ++ (show . numPlanetsInGalaxy) g ++ "\n" ++
     "Number of planets / star: " ++ (show2f . planetsPerStarsInGalaxy) g ++ "\n" ++
     "Minimum planet mass (Earths): " ++ (show . minPlanetMassInGalaxy) g ++ "\n" ++
     "Median planet mass (Earths): " ++ (show . medPlanetMassInGalaxy) g ++ "\n" ++
     "Average planet mass (Earths): " ++ (show . avgPlanetMassInGalaxy) g ++ "\n" ++
     "Maximum planet mass (Earths): " ++ (show . maxPlanetMassInGalaxy) g ++ "\n" ++
     "Minimum planet temperature (Celsius): " ++ (show . minPlanetTemperatureInGalaxy) g ++ "\n" ++
     "Median planet temperature (Celsius): " ++ (show . medPlanetTemperatureInGalaxy) g ++ "\n" ++
     "Average planet temperature (Celsius): " ++ (show . avgPlanetTemperatureInGalaxy) g ++ "\n" ++
     "Maximum planet temperature (Celsius): " ++ (show . maxPlanetTemperatureInGalaxy) g ++ "\n" ++
     showstars "Number of gas giants: " gasplanets planetstotal ++
     showstars "Number of oxygen planets: " oxyplanets planetstotal ++
     showstars "Number of planets without atmosphere: " noatmplanets planetstotal ++
     showstars "Number of habitable planets: " habitableplanets planetstotal

