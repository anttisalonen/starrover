module Main
where

import DataCreate
import GalaxyStats

main :: IO ()
main = browseRandomGalaxy

browseRandomGalaxy = do
  let g = testRandomGalaxy 20 256
  let stats = galaxyStats g
  putStrLn stats

