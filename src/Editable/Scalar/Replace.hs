{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Editable.Scalar.Replace where
import Editable.Core (Editable (apply), Invertable (invert), Rebasable (rebase))

data Replace a = Replace a a | NoChange

normalize :: Eq a => Replace a -> Replace a
normalize NoChange = NoChange
normalize o@(Replace f t)
    | f == t    = NoChange
    | otherwise = o

deriving instance Eq a => Eq (Replace a)

deriving instance Show a => Show (Replace a)

instance Eq a => Editable a (Replace a) where
    apply :: Replace a -> a -> Maybe a
    apply (Replace f t) x = if x == f then Just t else Nothing
    apply NoChange x      = Just x

instance Invertable (Replace a) where
    invert :: Replace a -> Replace a
    invert (Replace f t) = Replace t f
    invert NoChange      = NoChange

instance Ord a => Rebasable (Replace a) where
    rebase :: Ord a => Replace a -> Replace a -> Maybe (Replace a, Replace a)
    rebase NoChange o = Just (NoChange, o)
    rebase o NoChange = Just (o, NoChange)
    rebase (Replace f1 t1) (Replace f2 t2)
        | f1 /= f2  = Nothing
        | t1 < t2   = Just (NoChange, Replace t1 t2)
        | t1 > t2   = Just (Replace t2 t1, NoChange)
        | otherwise = Just (NoChange, NoChange)
