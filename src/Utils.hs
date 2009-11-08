module Utils
where

import Data.Maybe
import Control.Monad.State
import System.Random

import Math
import Statistics

firstMaybe :: (a -> Maybe b) -> [a] -> Maybe b
firstMaybe f l = listToMaybe $ mapMaybe f l

-- For all elements in snds, creates a new pair with fst and one of snd.
-- unroll [(1, [1,2,3]), (2, [3,4,5])] == [(1,1),(1,2),(1,3),(2,3),(2,4),(2,5)]
unroll :: [(a, [b])] -> [(a, b)]
unroll []           = []
unroll ((a, bs):xs) = (zip (repeat a) bs) ++ (unroll xs)

allEnums :: (Enum a, Bounded a) => [a]
allEnums = enumFrom minBound

mass :: Flt -> Flt -> Flt
mass density volume = density / volume

density :: Flt -> Flt -> Flt
density mass volume = mass / volume

round100 :: Int -> Int
round100 i = i `div` 100 * 100

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx v = if v < mn then mn else if v > mx then mx else v

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (h:_) = Just h

-- Separates values. The separation must be at least factor of 1.5.
-- separate [2,4,6,8,10] == [2,4,6,10]
-- separate [5..10] == [5,8]
separate :: (Fractional a, Ord a) => [a] -> [a]
separate []       = []
separate [x]      = [x]
separate (x:y:xs) = if x * 1.5 > y then separate (x:xs) else x:(separate (y:xs))

-- Distance to nearest neighbour.
-- distances [2, 3, 68, 70, 300, 4000] = [1, 1, 2, 2, 230, 3700]
distances :: (Num a, Ord a) => [a] -> [a]
distances []         = []
distances [a]        = [0]
distances [a, b]     = [b - a, b - a]
distances (a:b:c:ds) = b - a : go a b c ds
  where go a b c [] = min (b - a) (c - b) : c - b : []
        go a b c ds = min (b - a) (c - b) : go b c (head ds) (tail ds)

zipWith3M :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWith3M f [] _ _ = return []
zipWith3M f _ [] _ = return []
zipWith3M f _ _ [] = return []
zipWith3M f (a:as) (b:bs) (c:cs) = do
  n <- f a b c
  rest <- zipWith3M f as bs cs
  return (n:rest)

choose :: (RandomGen g) => [a] -> State g a
choose l = do
  let n = length l
  i <- randomRM (0, n - 1)
  return (l !! i)

