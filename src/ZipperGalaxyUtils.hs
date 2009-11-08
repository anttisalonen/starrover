{-# LANGUAGE TypeSynonymInstances #-}
module ZipperGalaxyUtils
where

import Data.Maybe
import Text.Printf
import qualified Data.Map as M

import ZipperGalaxy
import Galaxy
import DataFunction
import Named
import Utils

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

genInfo :: GalaxyZipper a -> String
genInfo z = galaxyInfo z ++ 
  (smallInfo z starSystemInZipper starSystemInfo) ++
  (smallInfo z starInZipper starInfo) ++
  (smallInfo z satelliteInZipper satelliteInfo)

galaxyInfo :: GalaxyZipper a -> String
galaxyInfo z = gname ++ "\n" ++ sysinfo
  where g       = galaxyInZipper z
        gname   = "Galaxy name: " ++ name g
        sysinfo = case starSystemInZipper z of
                    Just _  -> ""
                    Nothing -> getXInfoFromY g starsystems (genTitle sstitle)

starSystemInfo ss z = sysinfo ++ "\n" ++ starinfo
    where sysinfo = "Star system name: " ++ name ss
          starinfo = case starInZipper z of
                       Just _  -> ""
                       Nothing -> getXInfoFromY ss stars (genTitle startitle)

getXInfoFromY :: (Enum a, Num a) => b -> (b -> M.Map k c) -> (a -> c -> String) -> String
getXInfoFromY y getfunc printfunc = 
  let xs = (M.elems . getfunc) y
      titles = zipWith printfunc [1..] xs
  in concatMap (++ "\n") titles

sstitle :: StarSystem a -> String
sstitle ss = printf "%s - %d stars - %s" (name ss) (M.size (stars ss)) ((show . ssposition) ss)

genTitle :: (a -> String) -> Int -> a -> String
genTitle f num x = printf "%4d. %s" num (f x)

startitle s = printf 
    "%s - Type: %s - Temperature (Celsius): %6d - Orbit radius: %.3f - %d planets" 
      (name s) 
      (show (spectralType s)) 
      ((round100 . kelvinToCelsius . temperature) s) 
      ((orbitradius . starorbit) s)
      (M.size (planets s))

smallInfo :: a -> (a -> Maybe b) -> (b -> a -> String) -> String
smallInfo z getfunc printfunc = 
  let mx = getfunc z
  in case mx of
       Nothing -> ""
       Just x  -> printfunc x z

starInfo s z = starinfo ++ "\n" ++ planetinfo
  where starinfo = "Star name: " ++ name s
        planetinfo = case satelliteInZipper z of
                       Just _  -> ""
                       Nothing -> getXInfoFromY s planets (genTitle (infoSatellite z))

satelliteInfo s z = satinfo ++ satsinfo
  where satinfo = (infoSatellite z) s
        satsinfo = if (M.null (satellites s)) then "" else "\n" ++ getXInfoFromY s satellites (genTitle (flip satelliteInfo z))

infoSatellite :: GalaxyZipper a -> Planet a -> String
infoSatellite z p = 
  "Name: " ++ name p ++ " - " ++
  "Orbit radius: " ++ (show3f . orbitradius . orbit) p ++ " - " ++
  "Temperature (Celsius): " ++ show (kelvinToCelsius $ (planetTemperature (fromJust (starSystemInZipper z)) p)) ++ " - " ++
  "Mass: " ++ (show3f . bodymass . physics) p ++ " - " ++ 
  "Number of satellites: " ++ (show . M.size . satellites) p ++ " - " ++
  "Type: " ++ (show . planettype) p


