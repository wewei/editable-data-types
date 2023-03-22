{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

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

class WeakRebasable o where
    weakRebase  :: o -> o -> ([o], [o])
    weakRebase o1 o2 = (weakRebaseL o1 o2, weakRebaseL o2 o1)

    weakRebaseL :: o -> o -> [o]
    weakRebaseL = (fst .) . weakRebase

    weakRebaseR :: o -> o -> [o]
    weakRebaseR = flip weakRebaseL

class Rebasable o where
    rebase  :: o -> o -> (o, o)
    rebase o1 o2 = (o1 +> o2, o2 +> o1)

    (+>) :: o -> o -> o
    (+>) = (fst .) . rebase

    (<+) :: o -> o -> o
    (<+) = flip (+>)

instance Rebasable o => WeakRebasable o where
    weakRebase :: Rebasable o => o -> o -> ([o], [o])
    weakRebase o1 o2 = let (o3, o4) = rebase o1 o2 in ([o3], [o4])

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
instance WeakRebasable o => Rebasable [o] where
    rebase :: WeakRebasable o => [o] -> [o] -> ([o], [o])
    rebase [] xs = ([], xs)
    rebase xs [] = (xs, [])
    rebase (x:xs) (y:ys) = let
        (xs1, ys1) = weakRebase x y
        -- x <> ys1 == y <> xs1
        (xs2, ys3) = rebase xs ys1
        -- xs <> ys3 == ys1 <> xs2
        --    x <> xs <> ys3
        -- == x <> ys1 <> xs2
        -- == y <> xs1 <> xs2
        (xs3, ys2) = rebase xs1 ys
        -- xs1 <> ys2 == ys <> xs3
        (xs4, ys4) = rebase xs2 ys2
        -- xs2 <> ys4 == ys2 <> xs4
        --    x <> xs <> ys3 <> ys4
        -- == y <> xs1 <> xs2 <> ys4
        -- == y <> xs1 <> ys2 <> xs4
        -- == y <> ys <> xs3 <> xs4
        in (xs3 ++ xs4, ys3 ++ ys4)

newtype Rebase o = Rebase o;

instance Rebasable o => Rebasable (Rebase o) where
    (+>) :: Rebasable o => Rebase o -> Rebase o -> Rebase o
    (Rebase o1) +> (Rebase o2) = Rebase (o1 +> o2)

instance Eq o => Eq (Rebase o) where
  (==) :: Eq o => Rebase o -> Rebase o -> Bool
  (Rebase a) == (Rebase b) = a == b

instance Show o => Show (Rebase o) where
  showsPrec :: Show o => Int -> Rebase o -> ShowS
  showsPrec d (Rebase o) = showParen (d > 10) $
    showString "Rebase " . showsPrec 11 o

instance Rebasable o => Editable o (Rebase o) where
    apply :: Rebasable o => Rebase o -> o -> Maybe o
    apply (Rebase o) a = Just (a +> o)

