module Main
where

import Data.Maybe

import DataCreate
import GalaxyStats
import ZipperGalaxy
import ZipperGalaxyUtils

main :: IO ()
main = do
  let g = testRandomGalaxy 21 16
  putStrLn (galaxyStats g)
  browseGalaxy (g, Nothing)

type Input a = Maybe (GalaxyZipper a)

getInput :: GalaxyZipper a -> IO (Input a)
getInput z = do
  putStrLn (genInfo z)
  c <- getLine
  case reads c of
    [(num, _)] -> if num == 0 
                    then return $ Just (up z) 
                    else case tryDown num z of
                           Just nz -> return $ Just nz
                           Nothing -> getInput z
    _          -> if not (null c)
                    then case head c of
                           'q' -> return Nothing
                           's' -> putStrLn (galaxyStats (galaxyInZipper z)) >> getInput z
                           _   -> getInput z
                    else getInput z

browseGalaxy z = do
  i <- getInput z
  case i of
    Nothing -> return ()
    Just nz -> browseGalaxy nz

