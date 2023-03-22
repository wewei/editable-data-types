{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Editable.List.ReplaceSegment where
import Editable.Core (Editable (apply), Invertable (invert), Rebasable ((+>)))
import Data.List (intersperse)

data Eq a => ReplaceSegment a = ReplaceSegment
    { index :: Int
    , source :: [a]
    , target :: [a]
    } deriving Eq

instance (Show a, Eq a) => Show (ReplaceSegment a) where
    showsPrec :: (Show a, Eq a) => Int -> ReplaceSegment a -> ShowS
    showsPrec d (ReplaceSegment i s t) = showParen (d > 10) $
        foldl1 (.) $ intersperse (showChar ' ')
            [ showString "ReplaceSegment"
            , showsPrec 11 i
            , showsPrec 11 s
            , showsPrec 11 t ]

instance Eq a => Editable [a] (ReplaceSegment a) where
    apply :: Eq a => ReplaceSegment a -> [a] -> Maybe [a]
    apply (ReplaceSegment i s t) xs
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


instance Eq a => Invertable (ReplaceSegment a) where
    invert :: Eq a => ReplaceSegment a -> ReplaceSegment a
    invert (ReplaceSegment i s t) = ReplaceSegment i t s

instance Ord a => Rebasable (ReplaceSegment a) where
    (+>) :: Ord a => ReplaceSegment a -> ReplaceSegment a -> ReplaceSegment a
    (ReplaceSegment i1 s1 t1) +> (ReplaceSegment i2 s2 t2) = let
        [ls1, lt1, ls2, lt2] = map length [s1, t1, s2, t2]
        in
            if i1 < i2
                then if i1 + ls1 <= i2
                    then ReplaceSegment i1 s1 t1
                    else if i1 + ls1 <= i2 + ls2
                        then ReplaceSegment i1 (take (i2 - i1) s1) t1
                        else ReplaceSegment i1 (splice (i2 - i1) ls2 t2 s1) (t1 <> t2)
            else if i1 > i2
                then if i1 >= i2 + ls2
                    then ReplaceSegment (i1 - ls2 + lt2) s1 t1
                    else if i1 + ls1 > i2 + ls2
                        then ReplaceSegment (i2 + lt2) (drop (i2 + ls2 - i1) s1) t1
                        else ReplaceSegment (i2 + lt2) [] t1
            else if ls1 < ls2 || ls1 == ls2 && t1 <= t2
                then ReplaceSegment i1 [] t1
                else ReplaceSegment (i2 + lt2) (drop ls2 s1) t1
