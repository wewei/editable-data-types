{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Editable.Core where
import Data.Functor.Classes (Eq1 (liftEq), Show1 (liftShowsPrec))
import Control.Monad.Identity (Identity)

class Editable d o where
    apply :: o -> d -> Maybe d
    apply o d = Just d ~ o

    (~) :: Maybe d -> o -> Maybe d
    d ~ o = d >>= apply o

instance Semigroup m => Editable m m where
    apply :: Semigroup m => m -> m -> Maybe m
    apply x y = Just (x <> y)

instance (Editable d1 o1, Editable d2 o2) => Editable (d1, d2) (Either o1 o2) where
    apply :: (Editable d1 o1, Editable d2 o2) => Either o1 o2 -> (d1, d2) -> Maybe (d1, d2)
    apply (Left o) (d1, d2)  = (, d2) <$> apply o d1
    apply (Right o) (d1, d2) = (d1, ) <$> apply o d2

instance (Editable d o) => Editable d [o] where
    (~) :: Editable d o => Maybe d -> [o] -> Maybe d
    (~) = foldl (~)

class Invertable o where
    invert :: o -> o

instance (Invertable o) => Invertable [o] where
    invert :: Invertable o => [o] -> [o]
    invert = reverse . map invert

instance (Invertable o1, Invertable o2) => Invertable (Either o1 o2) where
    invert :: (Invertable o1, Invertable o2) => Either o1 o2 -> Either o1 o2
    invert (Left o)  = Left (invert o)
    invert (Right o) = Right (invert o)

class Rebasable o where
    -- We need to allow the rebase to fail.
    -- It's possible to receive operations from an unreliable endpoint, we need to protect the data
    -- structure from being corrupted by illegal operations.
    rebase  :: o -> o -> Maybe (o, o)

    (+>) :: Maybe o -> o -> Maybe o
    mo1 +> o2 = do
        o1  <- mo1
        fst <$> rebase o1 o2

    (<+) :: o -> Maybe o -> Maybe o
    (<+) = flip (+>)

{-
   +----------------+----------------+
   |       xs3      |       xs4      |
   |                |                |
ys |            ys2 |            ys4 |
   |                |                |
   |                |                |
   +----------------+----------------+
   |       xs1      |       xs2      |
   |                |                |
 y |            ys1 |            ys3 |
   |                |                |
   |                |                |
   +----------------+----------------+
            x               xs
-}

batchRebase :: (o -> o -> Maybe ([o], [o])) -> [o] -> [o] -> Maybe ([o], [o])
batchRebase weakRebase = rebase where
    rebase [] xs = Just ([], xs)
    rebase xs [] = Just (xs, [])
    rebase (x:xs) (y:ys) = do
        (xs1, ys1) <- weakRebase x y
        -- x <> ys1 == y <> xs1
        (xs2, ys3) <- rebase xs ys1
        -- xs <> ys3 == ys1 <> xs2
        --    x <> xs <> ys3
        -- == x <> ys1 <> xs2
        -- == y <> xs1 <> xs2
        (xs3, ys2) <- rebase xs1 ys
        -- xs1 <> ys2 == ys <> xs3
        (xs4, ys4) <- rebase xs2 ys2
        -- xs2 <> ys4 == ys2 <> xs4
        --    x <> xs <> ys3 <> ys4
        -- == y <> xs1 <> xs2 <> ys4
        -- == y <> xs1 <> ys2 <> xs4
        -- == y <> ys <> xs3 <> xs4
        return (xs3 ++ xs4, ys3 ++ ys4)

instance Rebasable o => Rebasable [o] where
    rebase :: Rebasable o => [o] -> [o] -> Maybe ([o], [o])
    rebase = batchRebase $ \x y -> do
        (x', y') <- rebase x y
        return ([x'], [y'])

newtype Rebase o = Rebase o;

deriving instance Rebasable o => Rebasable (Rebase o)

deriving instance Eq o => Eq (Rebase o)

deriving instance Show o => Show (Rebase o)

instance Rebasable o => Editable o (Rebase o) where
    (~) :: Rebasable o => Maybe o -> Rebase o -> Maybe o
    o1 ~ (Rebase o2) = o1 +> o2

