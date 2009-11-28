{-# LANGUAGE TypeSynonymInstances #-}
module ZipperGalaxyUtils
where

import Data.Maybe
import Text.Printf
import qualified Data.Map as M

import ZipperGalaxy
import Galaxy
import DataFunction
import Libaddutil.Named
import Utils
import Civilization

findZipperGalaxyToPlanet :: (Eq a) => Planet a -> Galaxy a -> Maybe (GalaxyZipper a)
findZipperGalaxyToPlanet p g = firstMaybe (findZipperStarSystemToPlanet p g) (M.elems (Galaxy.starsystems g))

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

genInfo :: (Show a) => GalaxyZipper a -> String
genInfo z = galaxyInfo z ++ 
  (smallInfo z starsystemInZipper starSystemInfo) ++
  (smallInfo z starInZipper starInfo) ++
  (smallInfo z satelliteInZipper satelliteInfo)

pplInfo :: [Empire] -> GalaxyZipper Terrain -> String
pplInfo es z = genInfo z

galaxyInfo :: (Show a) => GalaxyZipper a -> String
galaxyInfo z = gname ++ "\n" ++ sysinfo
  where g       = galaxyInZipper z
        gname   = "Galaxy name: " ++ name g
        sysinfo = case starsystemInZipper z of
                    Just _  -> ""
                    Nothing -> getXInfoFromY g Galaxy.starsystems (genTitle sstitle)

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
sstitle ss = printf "%s - %d stars - Main: %s - %s" 
    (name ss) 
    (M.size (stars ss)) 
    ((show . spectralType . snd . M.findMin) (stars ss)) 
    ((show . ssposition) ss)

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
                       Nothing -> getXInfoFromY s planets (genTitle infoSatellite)

satelliteInfo s z = satinfo ++ satsinfo ++ (if null datainfo then "" else "\n" ++ datainfo)
  where satinfo = infoSatellite s
        satsinfo = if (M.null (satellites s)) then "" else "\n" ++ getXInfoFromY s satellites (genTitle (flip satelliteInfo z))
        datainfo = show (info s)

infoSatellite :: (Show a) => Planet a -> String
infoSatellite p = 
  "Name: " ++ name p ++ " - " ++
  "Orbit radius: " ++ (show3f . orbitradius . orbit) p ++ " - " ++
  "Temperature (Celsius): " ++ show (kelvinToCelsius $ (planetTemperature p)) ++ " - " ++
  "Mass: " ++ (show3f . bodymass . physics) p ++ " - " ++ 
  "Number of satellites: " ++ (show . M.size . satellites) p ++ " - " ++
  "Type: " ++ (show . planettype) p

starsystems :: Galaxy a -> [GalaxyZipper a]
starsystems g = map (starsystemZipper g) (M.elems (Galaxy.starsystems g))

expandStarsystems :: [GalaxyZipper a] -> [GalaxyZipper a]
expandStarsystems = concatMap expandStarsystemZipper

expandStars :: [GalaxyZipper a] -> [GalaxyZipper a]
expandStars = concatMap expandStarZipper

expandPlanet :: [GalaxyZipper a] -> [GalaxyZipper a]
expandPlanet = concatMap expandPlanetZipper

allBodies :: Galaxy a -> [GalaxyZipper a]
allBodies = expandPlanet . expandStars . expandStarsystems . ZipperGalaxyUtils.starsystems

sustainsLifeZ :: GalaxyZipper a -> Bool
sustainsLifeZ (g, Just (ss, Just (s, (p:_)))) = sustainsLife p
sustainsLifeZ _                               = False

