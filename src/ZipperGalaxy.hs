{-# LANGUAGE FlexibleContexts #-}
module ZipperGalaxy
where

import Control.Monad
import qualified Data.Map as M
import Data.Maybe

import Libaddutil.Named

import Galaxy
import Utils
import qualified Data.Edison.Assoc.StandardMap as E

type GalaxyZipper a = (Galaxy a, Maybe (StarSystem a, Maybe (Star a, [Planet a])))

galaxyZipper :: Galaxy a -> GalaxyZipper a
galaxyZipper g = (g, Nothing)

starsystemZipper :: Galaxy a -> StarSystem a -> GalaxyZipper a
starsystemZipper g s = (g, Just (s, Nothing))

starZipper :: GalaxyZipper a -> Star a -> GalaxyZipper a
starZipper (g, Just (ss, _)) s = (g, Just (ss, Just (s, [])))
starZipper _                 _ = error "Invalid zipper"

expandStarsystemZipper :: GalaxyZipper a -> [GalaxyZipper a]
expandStarsystemZipper (g, Just (ss, _)) = map (\s -> (g, Just (ss, Just (s, [])))) (M.elems (stars ss))
expandStarsystemZipper _                 = []

expandStarZipper :: GalaxyZipper a -> [GalaxyZipper a]
expandStarZipper (g, Just (ss, Just (s, _))) = map (\p -> (g, Just (ss, Just (s, [p])))) (M.elems (planets s))
expandStarZipper _                           = []

expandPlanetZipper :: GalaxyZipper a -> [GalaxyZipper a]
expandPlanetZipper z@(g, Just (ss, Just (s, [p1]))) = z:map (\p2 -> (g, Just (ss, Just (s, p2:[p1])))) (M.elems (satellites p1))
expandPlanetZipper _                                = []

addPlanetToZipper :: GalaxyZipper a -> Planet a -> GalaxyZipper a
addPlanetToZipper (g, Just (ss, Just (s, ps))) p = (g, Just (ss, Just (s, p:ps)))
addPlanetToZipper _                            _ = error "Invalid zipper"

galaxyInZipper :: GalaxyZipper a -> Galaxy a
galaxyInZipper (g, _) = g

starsystemInZipper :: GalaxyZipper a -> Maybe (StarSystem a)
starsystemInZipper (_, Nothing)     = Nothing
starsystemInZipper (_, Just (s, _)) = Just s

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

tryDown :: Name -> GalaxyZipper a -> Maybe (GalaxyZipper a)
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

tryDownNum :: Int -> GalaxyZipper a -> Maybe (GalaxyZipper a)
tryDownNum i z@(g, Nothing) =
  case safeIndex (E.keys $ starsystems g) i of
    Nothing -> Nothing
    Just x  -> tryDown x z
tryDownNum i z@(g, Just (ss, Nothing)) =
  case safeIndex (E.keys $ stars ss) i of
    Nothing -> Nothing
    Just x  -> tryDown x z
tryDownNum i z@(g, Just (ss, Just (s, []))) =
  case safeIndex (E.keys $ planets s) i of
    Nothing -> Nothing
    Just x  -> tryDown x z
tryDownNum i z@(g, Just (ss, Just (s, (p:ps)))) =
  case safeIndex (E.keys $ satellites p) i of
    Nothing -> Nothing
    Just x  -> tryDown x z

up :: GalaxyZipper a -> GalaxyZipper a
up z@(g, Nothing) = z
up (g, Just (ss, Nothing)) = (g, Nothing)
up (g, Just (ss, Just (s, []))) = (g, Just (ss, Nothing))
up (g, Just (ss, Just (s, (sat:sats)))) = (g, Just (ss, Just (s, sats)))

type GalaxyLocation = [Name]

findLocation :: GalaxyLocation -> Galaxy a -> Maybe (Planet a)
findLocation l g = do
   let z = galaxyZipper g
   nz <- foldM (flip tryDown) z l
   satelliteInZipper nz

zipperToNames :: GalaxyZipper a -> [String]
zipperToNames z =
  let g  = galaxyInZipper z
      ss = starsystemInZipper z
      s  = starInZipper z
      ps = reverse $ satelliteThreadInZipper z
      ns = [name g, getName ss, getName s] ++ map name ps
      getName :: (Named a) => Maybe a -> String
      getName m = case m of
                    Nothing -> ""
                    Just x  -> name x
  in filter (not . null) ns


