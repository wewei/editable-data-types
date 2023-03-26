{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Editable.Map.Replace where
import Editable.Core (Invertable (invert), Editable (apply), Rebasable (rebase))
import Data.Map ( Map, alter, lookup )
import Prelude hiding (lookup)

data Replace k v = Replace k (Maybe v) (Maybe v) | NoChange

deriving instance (Show k, Show v) => Show (Replace k v)

deriving instance (Eq k, Eq v) => Eq (Replace k v)

normalize :: (Ord k, Eq v) => Replace k v -> Replace k v
normalize NoChange = NoChange
normalize o@(Replace k f t)
    | f == t    = NoChange
    | otherwise = o

instance Invertable (Replace k v) where
    invert :: Replace k v -> Replace k v
    invert (Replace k f t) = Replace k t f
    invert NoChange        = NoChange

instance (Ord k, Eq v) => Editable (Map k v) (Replace k v) where
    apply :: (Ord k, Eq v) => Replace k v -> Map k v -> Maybe (Map k v)
    apply NoChange d = Just d
    apply (Replace k f t) d
        | lookup k d == f = Just $ alter (const t) k d
        | otherwise       = Nothing

instance (Eq k, Ord v) => Rebasable (Replace k v) where
    rebase :: Ord v => Replace k v -> Replace k v -> Maybe (Replace k v, Replace k v)
    rebase NoChange o = Just (NoChange, o)
    rebase o NoChange = Just (o, NoChange)
    rebase o1@(Replace k1 f1 t1) o2@(Replace k2 f2 t2)
        | k1 == k2  =
            if f1 /= f2 then
                Nothing
            else if t1 > t2 then
                Just (Replace k1 t2 t1, NoChange)
            else if t1 < t2 then
                Just (NoChange, Replace k2 t1 t2)
            else
                Just (NoChange, NoChange)
        | otherwise = Just (o1, o2)
