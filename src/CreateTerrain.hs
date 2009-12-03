module CreateTerrain
where

import Data.Maybe
import Control.Monad.State
import System.Random

import Galaxy
import Math
import CreateGalaxy
import DataFunction
import Utils
import Statistics
import Good
import Terrain

nearsystems :: [String]
nearsystems = ["Alpha Centauri",
               "Barnard's Star",
               "Wolf 359",
               "Lalande 21185",
               "Sirius",
               "Lyuten 726-8",
               "Ross 154",
               "Ross 248",
               "Epsilon Eridani",
               "Lacaille 9352",
               "Ross 128",
               "EZ Aquarii",
               "Procyon",
               "61 Cygni",
               "Tau Ceti",
               "Fomalhaut",
               "Struve 2398",
               "Groombridge 34",
               "Epsilon Indi",
               "DX Cancri",
               "GJ 1061",
               "YZ Ceti", 
               "Lyuten's Star",
               "Kapteyn's Star"
              ]

testGalaxy :: Galaxy Terrain
testGalaxy = testRandomGalaxy 20 16

testRandomGalaxy :: Int -> Int -> Galaxy Terrain
testRandomGalaxy v numsys =
  let r = mkStdGen v
  in evalState (createGalaxy (createTerrain stdGoods) "milky way" (take numsys $ nearsystems ++ map show [1..numsys])) r

createTerrain :: [Good] -> Planet () -> Rnd Terrain
createTerrain gs p = 
  case planettype p of
    Planetoid         -> createRockyTerrain gs p
    NoAtmosphere      -> createRockyTerrain gs p
    RockyPlanet _     -> createRockyTerrain gs p
    SmallGasGiant     -> return (Terrain [] [])
    MediumGasGiant    -> return (Terrain [] [])
    LargeGasGiant     -> return (Terrain [] [])
    VeryLargeGasGiant -> return (Terrain [] [])

createRockyTerrain :: [Good] -> Planet () -> Rnd Terrain
createRockyTerrain gs p = do
  massmult <- randomRM (0, 1000 * planetMass p)
  gs' <- mapMaybeM (createNaturalGood massmult (planettype p)) gs
  return (Terrain gs' [])

createNaturalGood :: Flt -> PlanetType -> Good -> Rnd (Maybe (Resource, ResourceUnit))
createNaturalGood massmult pt g = 
  case natural g of
    Nothing                       -> return Nothing
    Just (Natural atms _ initial) -> do
      let atmNeeded = not $ null atms
      let invAtm = if not atmNeeded 
                     then False
                     else case pt of
                       RockyPlanet a -> a `notElem` atms
                       _             -> True
      if invAtm 
        then return Nothing
        else do
          mult <- randomRM (0, initial)
          let v = floor $ mult * massmult
          return $ Just ((g, v), v)


