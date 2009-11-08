module Statistics
where

import Control.Monad.State
import System.Random

randomRM :: (RandomGen g, Random a) => (a, a) -> State g a
randomRM v = do
  g <- get
  (x, g') <- return $ randomR v g
  put g'
  return x

stdNormal :: (RandomGen g, Random a, Ord a, Floating a) => State g a
stdNormal = do
  u1 <- randomRM (-1, 1)
  u2 <- randomRM (-1, 1)
  let m = stdNormalMarsaglia u1 u2
  case m of
    Nothing      -> stdNormal
    Just (z1, _) -> return z1

stdNormalMarsaglia :: (Ord a, Floating a) => a -> a -> Maybe (a, a)
stdNormalMarsaglia y1 y2 = 
  if q > 1 then Nothing else Just (z1, z2)
  where z1 = y1 * p
        z2 = y2 * p
        q = y1 * y1 + y2 * y2
        p = sqrt ((-2) * log q / q)

normal :: (RandomGen g, Random a, Ord a, Floating a) => a -> a -> State g a
normal mu sigma = do
  n <- stdNormal
  return $ mu + n * sigma

normalR :: (RandomGen g, Random a, Ord a, Floating a) => (a, a) -> a -> a -> State g a
normalR (mn, mx) mu sigma = do
  n <- normal mu sigma
  if n < mn 
    then return mn 
    else if n > mx
           then return mx else return n

normalIO :: (Random a, Ord a, Floating a) => a -> a -> IO a
normalIO mu sigma = newStdGen >>= return . evalState (normal mu sigma)

normalRIO :: (Random a, Ord a, Floating a) => (a, a) -> a -> a -> IO a
normalRIO limits mu sigma = newStdGen >>= return . evalState (normalR limits mu sigma)

average :: (Fractional a) => [a] -> a
average l = go 0 0 l
  where go acc len []     = acc / len
        go acc len (x:xs) = go (acc + x) (len + 1) xs

averageInt :: [Int] -> Int
averageInt l = go 0 0 l
  where go acc len []     = acc `div` len
        go acc len (x:xs) = go (acc + x) (len + 1) xs

median :: (Num a) => [a] -> a
median [] = 0
median (x:xs) = go 0 x xs
   where go _ x []     = x
         go 0 x (n:ns) = go 1 x ns
         go 1 x (n:ns) = go 1 n ns


