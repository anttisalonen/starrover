{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module DB
where

import qualified Data.Map as M

class DB a k v where
  lookup :: (Ord k) => a k v -> k -> Maybe v
  empty :: a k v
  member :: (Ord k) => a k v -> k -> Bool
  toList :: a k v -> [(k, v)]
  keys :: a k v -> [k]
  elems :: a k v -> [v]

instance DB M.Map k v where
  lookup = flip M.lookup
  empty = M.empty
  member = flip M.member
  toList = M.toList
  keys = M.keys
  elems = M.elems
