module Main
where

import Text.Printf
import qualified Data.Map as M

import DataCreate
import GalaxyStats
import DataTypes
import ZipperGalaxy

main :: IO ()
main = do
  let g = testRandomGalaxy 20 8
  let stats = galaxyStats g
  putStrLn (galaxyStats g)
  browseGalaxy (g, Nothing)

type Input a = Maybe (GalaxyZipper a)

sstitle :: Int -> StarSystem a -> String
sstitle num ss = printf "%4d. %s - %d stars - %s" num (name ss) (M.size (stars ss)) ((show . ssposition) ss)

getInput :: GalaxyZipper a -> IO (Input a)
getInput z = do
  c <- getLine
  case reads c of
    [(num, _)] -> case tryDown num z of
                    Just nz -> return $ Just nz
                    Nothing -> getInput z
    _          -> if not (null c) && head c == 'q' 
                    then return Nothing 
                    else getInput z

ssFromGal g =
  let ss = (M.elems . starsystems) g
      titles = zipWith sstitle [1..] ss
  in concatMap (++ "\n") titles

genInfo :: GalaxyZipper a -> String
genInfo z = galaxyInfo z ++ starSystemInfo z ++ starInfo z ++ satelliteInfo z

galaxyInfo :: GalaxyZipper a -> String
galaxyInfo z = gname ++ "\n" ++ sysinfo
  where g       = galaxyInZipper z
        gname   = "Galaxy name: " ++ name g
        sysinfo = case starSystemInZipper z of
                    Just _  -> ""
                    Nothing -> ssFromGal g

starSystemInfo z = 
  let mss = starSystemInZipper z
  in case mss of
       Nothing -> ""
       Just ss -> starSystemInfo' ss

starSystemInfo' ss = sysinfo ++ starinfo
    where sysinfo = name ss
          starinfo = ""

starInfo z = ""
satelliteInfo z = ""

infoSatellite :: Planet a -> String
infoSatellite s = 
  "Name: " ++ name s ++ "\n" ++
  "Orbit radius: " ++ (show3f . orbitradius . orbit) s ++ "\n" ++
  "Mass: " ++ (show3f . bodymass . physics) s ++ "\n" ++ 
  "Type: " ++ (show . planettype) s

browseGalaxy g = do
  putStrLn $ genInfo g
  i <- getInput g
  case i of
    Nothing -> return ()
    Just x  -> return ()

