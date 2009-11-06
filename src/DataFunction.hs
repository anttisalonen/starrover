module DataFunction
where

import DataTypes

mass :: Flt -> Flt -> Flt
mass density volume = density / volume

density :: Flt -> Flt -> Flt
density mass volume = mass / volume

minOTemp = 30000
minBTemp = 10000
minATemp = 7500
minFTemp = 6000
minGTemp = 5200
minKTemp = 3700
minMTemp = 0

specTemps = [minOTemp, minBTemp, minATemp, minFTemp, minGTemp, minKTemp, minMTemp]

allEnums :: (Enum a, Bounded a) => [a]
allEnums = enumFrom minBound

tempTable :: [(SpectralType, Temperature)]
tempTable = zip allEnums specTemps

temperatureToSpectralType :: Temperature -> SpectralType
temperatureToSpectralType t = (fst . head . filter (\(st, t') -> t >= t')) tempTable

spectralType :: Star a -> SpectralType
spectralType s = temperatureToSpectralType (temperature s)

kelvinToCelsius :: Temperature -> Temperature
kelvinToCelsius = subtract 273

planetTemperature'' :: Temperature -> Flt -> Atmosphere -> Temperature
planetTemperature'' t d a = floor $ 300 * (((fromIntegral t) / 6000) ^ 2) * (d ** (-0.7)) * acoeff
   where acoeff = case a of
           NoAtmosphere -> 0.8
           Nitrogen -> 0.9
           CarbonDioxide -> 0.95
           Oxygen -> 1.0
           Methane -> 1.5
           GasGiant -> 1.0

planetTemperature' :: Temperature -> Planet a -> Temperature
planetTemperature' t planet =
  planetTemperature''
    t
    (orbitradius (orbit planet)) 
    (atmosphere planet)

planetTemperature :: Star a -> Planet a -> Temperature -- TODO: take other stars in starsystem into account
planetTemperature star = planetTemperature' (temperature star)

sustainsLife :: Star a -> Planet a -> Bool
sustainsLife s p = 
  atmosphere p == Oxygen && 
  planetMass p > 0.01  && 
  planetMass p < 20.0  && 
  planetTemperature s p < 3300 && 
  planetTemperature s p > 80 -- TODO: improve

planetMass :: Planet a -> Flt
planetMass = bodymass . physics

starPlanetPairs :: Galaxy a -> [(Star a, Planet a)]
starPlanetPairs g = unroll (zip (allStars g) (map planets (allStars g)))

allStars :: Galaxy a -> [Star a]
allStars g = concatMap stars (starsystems g)

-- unroll [(1, [1,2,3]), (2, [3,4,5])] == [(1,1),(1,2),(1,3),(2,3),(2,4),(2,5)]
unroll :: [(a, [b])] -> [(a, b)]
unroll []           = []
unroll ((a, bs):xs) = (zip (repeat a) bs) ++ (unroll xs)

planetTemperatures :: Star a -> [Temperature]
planetTemperatures s = map (planetTemperature s) (planets s)


