{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module Civilization
where

import Data.Maybe
import Text.Printf

import qualified Data.Edison.Assoc.StandardMap as E

import Libaddutil.Named

import Galaxy
import ZipperGalaxy
import Utils
import Good
import Terrain

type CivKey = Name

data Empire = Empire { empirename :: String
                     , colonies   :: E.FM CivKey Colony
                     }

instance Show Empire where
  show = dispEmpire

dispEmpire :: Empire -> String
dispEmpire e = printf "%s - %d colonies" (name e) (E.size (colonies e))

dispColoniesInfo :: Empire -> String
dispColoniesInfo = concatMap dispColony . E.elements . colonies

type Inventory = E.FM GoodName ResourceUnit

transaction :: GoodName -> ResourceUnit -> Inventory -> Inventory
transaction g u i = E.adjust (+u) g i

stdColony :: String -> ResourceUnit -> GalaxyLocation -> Colony
stdColony n p l = Colony n p l E.empty

data Colony = Colony { colonyname :: String
                     , population :: ResourceUnit
                     , location   :: GalaxyLocation
                     , market     :: Inventory
                     }

dispColony :: Colony -> String
dispColony c = printf "\t%s - %s - %s" (name c) (show (population c)) (show (location c))

instance Show Colony where
  show = dispColony

instance Named Colony where
  name = colonyname

instance Named Empire where
  name = empirename

terrainInfo :: E.FM CivKey Empire -> Terrain -> String
terrainInfo es t = show t ++
  case findColony es (colony t) of 
    Nothing         -> ""
    Just (emp, col) -> "\n\t" ++ show emp ++ show col

findColony :: E.FM CivKey Empire -> EmpireLocation -> Maybe (Empire, Colony)
findColony _ []     = Nothing
findColony m (e:es) = do
  n <- E.lookupM e m
  c <- findColony' (colonies n) es
  return (n, c)

findColony' :: E.FM CivKey Colony -> EmpireLocation -> Maybe Colony
findColony' _ []    = Nothing
findColony' m (e:_) = E.lookupM e m

setColony :: EmpireLocation -> Terrain -> Terrain
setColony f t = t{colony = f}

setColonyOnPlanet :: EmpireLocation -> Name -> Galaxy Terrain -> Galaxy Terrain
setColonyOnPlanet f n = updateStarsystems $ updateStars $ updateBodies $ (\p -> if name p == n then (updateTerrain (setColony f) p) else p)


