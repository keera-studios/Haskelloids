module Data.IdentityList (IList,
                          ILKey,
                          empty,
                          insert,
                          insertMany,
                          fromList,
                          elems,
                          assocs,
                          delete,
                          deleteMany,
                          mapWithKey
                         ) where

import Data.Word (Word32)

import qualified Data.Map as Map (Map, empty, insert, lookup, elems, assocs,
                                  delete, mapWithKey)

type ILKey = Word32 -- NB: maximum of 2^32 objects can be inserted after initialisation

data IList a = IL {
  ilNextKey :: ILKey,
  ilAssocs  :: Map.Map ILKey a
} deriving (Show)

empty :: IList a
empty = IL { ilNextKey = minBound,
             ilAssocs = Map.empty  }

insert :: IList a -> a -> IList a
insert (IL nxtKy assc) e = IL (succ nxtKy) assc'
  where assc' = Map.insert nxtKy e assc

insertMany :: IList a -> [a] -> IList a
insertMany = foldl insert

fromList :: [a] -> IList a
fromList = insertMany empty

elems :: IList a -> [a]
elems = Map.elems . ilAssocs

assocs :: IList a -> [(ILKey, a)]
assocs = Map.assocs . ilAssocs

delete :: IList a -> ILKey -> IList a
delete (IL nxtKy map) k = IL nxtKy (Map.delete k map)

deleteMany :: IList a -> [ILKey] -> IList a
deleteMany = foldl delete

mapWithKey :: (ILKey -> a -> b) -> IList a -> IList b
mapWithKey f il = il { ilAssocs = Map.mapWithKey f . ilAssocs $ il }

instance Functor IList where
  fmap f (IL nxtKy assc) = IL nxtKy (fmap f assc)
