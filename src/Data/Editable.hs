{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Editable where
import Data.Functor.Identity (Identity)
import Control.Monad (foldM)

class Editable d o where
    apply :: o -> d -> Maybe d

instance Monoid m => Editable m m where
    apply :: Monoid m => m -> m -> Maybe m
    apply x y = Just (x <> y)

instance (Editable d1 o1, Editable d2 o2) => Editable (d1, d2) (Either o1 o2) where
    apply :: (Editable d1 o1, Editable d2 o2) => Either o1 o2 -> (d1, d2) -> Maybe (d1, d2)
    apply (Left o) (d1, d2)  = (, d2) <$> apply o d1
    apply (Right o) (d1, d2) = (d1, ) <$> apply o d2

instance (Editable d o1, Editable d o2) => Editable d (Either o1 o2) where
    apply :: (Editable d o1, Editable d o2) => Either o1 o2 -> d -> Maybe d
    apply (Left o) d  = apply o d
    apply (Right o) d = apply o d

instance (Editable d o) => Editable d [o] where
    apply :: Editable d o => [o] -> d -> Maybe d
    apply = flip $ foldM $ flip apply
