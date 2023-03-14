{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}

module Data.Rebasable where
import Data.List (mapAccumL)
import Data.Editable (Editable (apply))
import Control.Monad

class WeakRebasable o where
    weakRebase  :: o -> o -> ([o], [o])
    weakRebase o1 o2 = (weakRebaseL o1 o2, weakRebaseL o2 o1)

    weakRebaseL :: o -> o -> [o]
    weakRebaseL = (fst .) . weakRebase

    weakRebaseR :: o -> o -> [o]
    weakRebaseR = flip weakRebaseL

class Rebasable o where
    rebase  :: o -> o -> (o, o)
    rebase o1 o2 = (rebaseL o1 o2, rebaseL o2 o1)

    rebaseL :: o -> o -> o
    rebaseL = (fst .) . rebase

    rebaseR :: o -> o -> o
    rebaseR = flip rebaseL

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
        in (xs3 <> xs4, ys3 <> ys4)

checkCP1 :: (Eq d, WeakRebasable o, Editable d o) => o -> o -> d -> (Bool, (Maybe d, Maybe d))
checkCP1 o1 o2 d = let
    (os1, os2) = weakRebase o1 o2
    d1 = foldM (flip apply) d (o1:os2)
    d2 = foldM (flip apply) d (o2:os1)
    in (d1 == d2, (d1, d2))
