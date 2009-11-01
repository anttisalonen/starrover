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

spectralType :: Star -> SpectralType
spectralType s = temperatureToSpectralType (temperature s)

