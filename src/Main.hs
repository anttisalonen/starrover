module Main
where

import Data.Maybe

import DataCreate
import GalaxyStats
import DataFunction
import ZipperGalaxy
import Galaxy
import World
import Civilization
import ZipperGalaxyUtils

main :: IO ()
main = do
  let g = testRandomGalaxy 22 24
  let w = testRandomWorld g 22 1
  putStrLn (galaxyStats g)
  print testPlanetCreatingZipper
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
    Life   -> browseLife w >>= mainMenu
    Zipper -> browseGalaxy (g, Nothing) >> mainMenu w

data MainMenuInput = Zipper | Life | Quit

browseLife :: World -> IO World
browseLife w = do
  i <- getLifeInput w
  case i of
    Nothing -> return w
    Just v  -> browseLife (timePass v w)

lifeInfo :: World -> String
lifeInfo w = ""

getLifeInput :: World -> IO (Maybe Int)
getLifeInput w = do
  putStrLn (lifeInfo w)
  c <- getLine
  case reads c of
    [(num, _)] -> return (Just num)
    _          -> return Nothing

timePass :: Int -> World -> World
timePass i w | i <= 0    = w
             | otherwise =
  let newgal = regenerateGalaxy 0.5 (galaxy w)
  in timePass (i - 1) (w{galaxy = newgal})

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

getZipperInput :: (Show a) => GalaxyZipper a -> IO (ZipperInput a)
getZipperInput z = do
  putStrLn (genInfo z)
  c <- getLine
  case length c of
    1          -> if not (null c)
                    then case head c of
                           'q' -> return Nothing
                           's' -> putStrLn (galaxyStats (galaxyInZipper z)) >> getZipperInput z
                           _   -> getZipperInput z
                    else getZipperInput z
    _          -> if c == ""
                    then return $ Just (up z) 
                    else case tryDown c z of
                           Just nz -> return $ Just nz
                           Nothing -> getZipperInput z

browseGalaxy :: (Show a) => GalaxyZipper a -> IO ()
browseGalaxy z = do
  i <- getZipperInput z
  case i of
    Nothing -> return ()
    Just nz -> browseGalaxy nz

testPlanetCreatingZipper :: Bool
testPlanetCreatingZipper =
  let g = testRandomGalaxy 22 24
      n1 = map planetname $ map fromJust $ map ZipperGalaxy.satelliteInZipper (habitablePlanetsZ g)
      n2 = map planetname (filter sustainsLife (DataFunction.allBodies g))
  in n1 == n2

