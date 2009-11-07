module Main
where

import Text.Printf
import qualified Data.Map as M

import DataCreate
import DataFunction
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

getInput :: GalaxyZipper a -> IO (Input a)
getInput z = do
  c <- getLine
  case reads c of
    [(num, _)] -> if num == 0 
                    then return $ Just (up z) 
                    else case tryDown num z of
                           Just nz -> return $ Just nz
                           Nothing -> getInput z
    _          -> if not (null c) && head c == 'q' 
                    then return Nothing 
                    else getInput z

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

startitle s = printf "%s - Type: %s - Temperature: %6d - %d planets" (name s) (show (spectralType s)) (temperature s) (M.size (planets s))

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

satelliteInfo s z = ""

infoSatellite :: Planet a -> String
infoSatellite s = 
  "Name: " ++ name s ++ " - " ++
  "Orbit radius: " ++ (show3f . orbitradius . orbit) s ++ " - " ++
  "Mass: " ++ (show3f . bodymass . physics) s ++ " - " ++ 
  "Type: " ++ (show . planettype) s

browseGalaxy g = do
  putStrLn $ genInfo g
  i <- getInput g
  case i of
    Nothing -> return ()
    Just nz -> browseGalaxy nz

