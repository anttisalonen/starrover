module Main
where

import Data.Maybe
import Control.Monad

import EmpireZipper
import CreateTerrain
import Terrain
import GalaxyStats
import DataFunction
import ZipperGalaxy
import Galaxy
import World
import Civilization
import ZipperGalaxyUtils
import Math
import Utils
import qualified Data.Edison.Assoc.StandardMap as E

main :: IO ()
main = do
  let g = testRandomGalaxy 22 24
  let w = testRandomWorld g 22 5
  putStrLn (galaxyStats g)
  let tplc = testPlanetCreatingZipper
  when (not tplc) $ putStrLn "!!! Warning: zipper test failed!"
  case w of
    Nothing -> putStrLn "Unable to create world?"
    Just jw -> do
      mainMenu jw

mainMenu :: World -> IO ()
mainMenu w = do
  let g = galaxy w
  i <- getMainMenuInput
  case i of
    Quit   -> return ()
    Life   -> lifeMenu w >>= mainMenu
    Zipper -> browseGalaxy' (empires w) (g, Nothing) >> mainMenu w

data MainMenuInput = Zipper | Life | Quit

lifeMenu :: World -> IO World
lifeMenu w = do
  i <- getLifeInput w
  case i of
    QuitLife   -> return w
    Rounds v   -> lifeMenu (timePass v w)
    BrowseLife -> browseLife (newEmpireZipper w) >> lifeMenu w

browseLife :: EmpireZipper -> IO ()
browseLife z = do
  putStrLn $ empireZipperInfo z
  mz <- getBrowseLifeInput z
  case mz of
    Nothing -> return ()
    Just nz -> browseLife nz

empireZipperInfo :: EmpireZipper -> String
empireZipperInfo (es, Nothing) = lifeInfo es
empireZipperInfo (es, Just (e, Nothing)) = dispEmpire e ++ "\n" ++ dispColoniesInfo e
empireZipperInfo (es, Just (e, Just c)) = dispColony c

getBrowseLifeInput :: EmpireZipper -> IO (Maybe EmpireZipper)
getBrowseLifeInput z = do
  putStrLn "Name of thing to inspect"
  c <- getLine
  if null c 
    then return Nothing
    else if c == "up" 
           then return $ Just $ upEZ z
           else case tryDownEZ c z of
                  Nothing -> do
                               putStrLn "Not found"
                               getBrowseLifeInput z
                  Just nz -> do
                               putStrLn "Found way down"
                               return $ Just nz

lifeInfo :: E.FM CivKey Empire -> String
lifeInfo = concat . map (++ "\n") . E.elements . E.map dispEmpire

data LifeInput = Rounds Int | BrowseLife | QuitLife

getLifeInput :: World -> IO LifeInput
getLifeInput w = do
  putStrLn (lifeInfo (empires w))
  putStrLn "Type in:"
  putStrLn " - Number to run rounds"
  putStrLn " - nothing to go back"
  putStrLn " - anything else to browse life"
  c <- getLine
  case reads c of
    [(num, _)] -> return (Rounds num)
    _          -> return $ if null c then QuitLife else BrowseLife

timePass :: Int -> World -> World
timePass i w | i <= 0    = w
             | otherwise =
  let newgal  = regenerateGalaxy 0.05 (galaxy w)
      newemps = E.map (updateEmpire 1) (empires w)
  in timePass (i - 1) (w{galaxy = newgal, empires = newemps})

updateEmpire :: Flt -> Empire -> Empire
updateEmpire t e = 
  let newcol = E.map (popgrow t) (colonies e)
  in e{colonies = newcol}

popgrow :: Flt -> Colony -> Colony
popgrow t c = 
  let oldpop = population c
      newpop = floor (t * 1.1 * fromIntegral oldpop)
  in c{population = newpop}

getMainMenuInput :: IO MainMenuInput
getMainMenuInput = do
  putStrLn "What would you like to do?"
  putStrLn "l. Life"
  putStrLn "g. Browse galaxy"
  putStrLn "_. Quit"
  s <- getLine
  case s of
    [] -> getMainMenuInput
    (c:cs) -> return $ case c of
                         'l' -> Life
                         'g' -> Zipper
                         _   -> Quit

type ZipperInput a = Maybe (GalaxyZipper a)

getZipperInput :: (Show a) => (GalaxyZipper a -> String) -> GalaxyZipper a -> IO (ZipperInput a)
getZipperInput showfunc z = do
  putStrLn $ showfunc z
  c <- getLine
  let num :: Maybe Int
      num = liftM fst $ safeHead (reads c)
  case num of
    Nothing -> strInput c
    Just n  -> numInput n

 where 
   strInput c = do
    case length c of
      1          -> if not (null c)
                      then case head c of
                             'q' -> return Nothing
                             's' -> putStrLn (galaxyStats (galaxyInZipper z)) >> getZipperInput showfunc z
                             _   -> getZipperInput showfunc z
                      else getZipperInput showfunc z
      _          -> if c == ""
                      then return $ Just (up z) 
                      else proceed c z

   numInput n = do
     case tryDownNum (n - 1) z of
       Just nz -> return $ Just nz
       Nothing -> return $ Just (up z)

   proceed c z = case tryDown c z of
                   Just nz -> return $ Just nz
                   Nothing -> getZipperInput showfunc z

browseGalaxy :: (Show a) => GalaxyZipper a -> IO ()
browseGalaxy = someInput (getZipperInput (genInfo (\t -> show t)))

someInput :: (a -> IO (Maybe a)) -> a -> IO ()
someInput func x = do
  i <- func x
  case i of
    Nothing -> return ()
    Just nx -> someInput func nx

browseGalaxy' :: E.FM CivKey Empire -> GalaxyZipper Terrain -> IO ()
browseGalaxy' e = someInput (getZipperInput (genInfo (terrainInfo e)))

testPlanetCreatingZipper :: Bool
testPlanetCreatingZipper =
  let g = testRandomGalaxy 22 24
      n1 = map planetname $ map fromJust $ map ZipperGalaxy.satelliteInZipper (habitablePlanetsZ g)
      n2 = map planetname (filter sustainsLife (DataFunction.allBodies g))
  in n1 == n2

