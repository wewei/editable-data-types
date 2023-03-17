{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Editable.ReplaceValue where

import Data.Editable ( Editable(..) )
import Data.Invertable ( Invertable(..) )

data Eq a => ReplaceValue a = ReplaceValue
    { from :: a
    , to :: a
    }

instance Eq a => Editable a (ReplaceValue a) where
    apply :: ReplaceValue a -> a -> Maybe a
    apply (ReplaceValue f t) x = if x == f then Just t else Nothing

instance Eq a => Invertable (ReplaceValue a) where
    invert :: ReplaceValue a -> ReplaceValue a
    invert (ReplaceValue f t) = ReplaceValue t f
