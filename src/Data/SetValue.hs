{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Data.SetValue where
import Data.Editable (Editable (apply))

newtype SetValue a = SetValue a

instance Editable a (SetValue a) where
    apply :: SetValue a -> a -> Maybe a
    apply (SetValue x) _ = Just x
