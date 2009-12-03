module Terrain
where

import Data.Maybe

import Galaxy
import Math
import Utils
import Good

type EmpireLocation = [Name]

data Terrain = Terrain { terraingoods :: [(Resource, ResourceUnit)]
                       , colony       :: EmpireLocation }
    deriving (Eq)

instance Show Terrain where
  show t = "    " ++ concatMap ((++ " ") . show) (terraingoods t)

regenerate :: Flt -> Terrain -> Terrain
regenerate coeff t = let nr = map (regenRes coeff) (terraingoods t)
                     in t{terraingoods = nr}

regenRes :: Flt -> (Resource, ResourceUnit) -> (Resource, ResourceUnit)
regenRes coeff r@(_, m) = (regenerateGood coeff r, m)

regenerateGood :: Flt -> (Resource, ResourceUnit) -> Resource
regenerateGood coeff (r@(g, v), m) = 
  case natural g of
    Nothing -> r
    Just n  -> if v == 0 
                 then r
                 else if growthRate n == 0
                        then r
                        else (g, min m (floor (fromIntegral v * (1 + coeff * growthRate n))))

updateTerrain :: (a -> a) -> Planet a -> Planet a
updateTerrain f p = let oldinfo = info p
                        newinfo = f oldinfo
                    in p{info = newinfo}

regenerateGalaxy :: Flt -> Galaxy Terrain -> Galaxy Terrain
regenerateGalaxy coeff = updateStarsystems $ updateStars $ updateBodies $ updateTerrain $ regenerate coeff


