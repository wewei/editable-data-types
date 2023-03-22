{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Editable.List.Replace where
import Editable.Core (Editable (apply), Invertable (invert), Rebasable ((+>)))
import Data.List (intersperse)

data Eq a => Replace a = Replace
    { index :: Int
    , source :: [a]
    , target :: [a]
    } deriving Eq

instance (Show a, Eq a) => Show (Replace a) where
    showsPrec :: (Show a, Eq a) => Int -> Replace a -> ShowS
    showsPrec d (Replace i s t) = showParen (d > 10) $
        foldl1 (.) $ intersperse (showChar ' ')
            [ showString "Replace"
            , showsPrec 11 i
            , showsPrec 11 s
            , showsPrec 11 t ]

instance Eq a => Editable [a] (Replace a) where
    apply :: Eq a => Replace a -> [a] -> Maybe [a]
    apply (Replace i s t) xs
        | i >= 0 && i <= length xs = let
            (prefix, ys) = splitAt i xs
            (ws, suffix) = splitAt (length s) ys
            in if ws == s
                then Just $ prefix <> t <> suffix
                else Nothing
        | otherwise               = Nothing

splice :: Int -> Int -> [a] -> [a] -> [a]
splice i l xs ys = let
    (prefix, zs) = splitAt i ys
    (ws, suffix) = splitAt l zs
    in prefix <> xs <> suffix


instance Eq a => Invertable (Replace a) where
    invert :: Eq a => Replace a -> Replace a
    invert (Replace i s t) = Replace i t s

instance Ord a => Rebasable (Replace a) where
    (+>) :: Ord a => Replace a -> Replace a -> Replace a
    (Replace i1 s1 t1) +> (Replace i2 s2 t2) = let
        [ls1, lt1, ls2, lt2] = map length [s1, t1, s2, t2]
        in
            if i1 < i2
                then if i1 + ls1 <= i2
                    then Replace i1 s1 t1
                    else if i1 + ls1 <= i2 + ls2
                        then Replace i1 (take (i2 - i1) s1) t1
                        else Replace i1 (splice (i2 - i1) ls2 t2 s1) (t1 <> t2)
            else if i1 > i2
                then if i1 >= i2 + ls2
                    then Replace (i1 - ls2 + lt2) s1 t1
                    else if i1 + ls1 > i2 + ls2
                        then Replace (i2 + lt2) (drop (i2 + ls2 - i1) s1) t1
                        else Replace (i2 + lt2) [] t1
            else if ls1 < ls2 || ls1 == ls2 && t1 <= t2
                then Replace i1 [] t1
                else Replace (i2 + lt2) (drop ls2 s1) t1
