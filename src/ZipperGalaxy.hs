module ZipperGalaxy
where

import qualified Data.Map as M

import Galaxy
import DataFunction
import Utils

type GalaxyZipper a = (Galaxy a, Maybe (StarSystem a, Maybe (Star a, [Planet a])))

galaxyInZipper :: GalaxyZipper a -> Galaxy a
galaxyInZipper (g, _) = g

starSystemInZipper :: GalaxyZipper a -> Maybe (StarSystem a)
starSystemInZipper (_, Nothing)     = Nothing
starSystemInZipper (_, Just (s, _)) = Just s

starInZipper :: GalaxyZipper a -> Maybe (Star a)
starInZipper (_, Nothing) = Nothing
starInZipper (_, Just (ss, Nothing)) = Nothing
starInZipper (_, Just (ss, Just (s, _))) = Just s

satelliteThreadInZipper :: GalaxyZipper a -> [Planet a]
satelliteThreadInZipper (_, Nothing) = []
satelliteThreadInZipper (_, Just (ss, Nothing)) = []
satelliteThreadInZipper (_, Just (ss, Just (s, sats))) = sats

satelliteInZipper :: GalaxyZipper a -> Maybe (Planet a)
satelliteInZipper z = safeHead (satelliteThreadInZipper z)

tryDown :: Int -> GalaxyZipper a -> Maybe (GalaxyZipper a)
tryDown i (g, Nothing) = 
  let mss = M.lookup i (starsystems g)
  in case mss of
       Nothing -> Nothing
       Just x  -> Just $ (g, Just (x, Nothing))
tryDown i (g, Just (ss, Nothing)) =
  let ms = M.lookup i (stars ss)
  in case ms of
       Nothing -> Nothing
       Just x  -> Just $ (g, Just (ss, (Just (x, []))))
tryDown i (g, Just (ss, Just (s, []))) =
  let msat = M.lookup i (planets s)
  in case msat of
       Nothing -> Nothing
       Just x  -> Just $ (g, Just (ss, (Just (s, [x]))))
tryDown i (g, Just (ss, Just (s, allsats@(sat:sats)))) =
  let msat = M.lookup i (satellites sat)
  in case msat of
       Nothing -> Nothing
       Just x  -> Just $ (g, Just (ss, (Just (s, x:allsats))))

up :: GalaxyZipper a -> GalaxyZipper a
up z@(g, Nothing) = z
up (g, Just (ss, Nothing)) = (g, Nothing)
up (g, Just (ss, Just (s, []))) = (g, Just (ss, Nothing))
up (g, Just (ss, Just (s, (sat:sats)))) = (g, Just (ss, Just (s, sats)))

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

