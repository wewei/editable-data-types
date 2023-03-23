{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Editable.List.Replace where
import Editable.Core (Editable (apply), Invertable (invert), Rebasable ((+>), rebase))
import Data.List (intersperse)
import Editable.List.Util (splice)

data Replace a
    = Replace Int [a] [a]
    | NoChange deriving Eq

normalize :: Eq a => Replace a -> Replace a
normalize NoChange          = NoChange
normalize (Replace _ [] []) = NoChange
normalize (Replace i [] t)  = Replace i [] t
normalize (Replace i s [])  = Replace i s []
normalize (Replace i s@(x:xs) t@(y:ys))
    | x == y    = normalize $ Replace (i + 1) xs ys
    | otherwise = Replace i s' t' where
        (s', t')   = rtrim (reverse s) (reverse t)
        rtrim [] t = ([], reverse t)
        rtrim s [] = (reverse s, [])
        rtrim s@(x:xs) t@(y:ys)
            | x == y    = rtrim xs ys
            | otherwise = (reverse s, reverse t)
        

instance (Show a) => Show (Replace a) where
    showsPrec :: (Show a) => Int -> Replace a -> ShowS
    showsPrec d (Replace i s t) = showParen (d > 10) $
        foldl1 (.) $ intersperse (showChar ' ')
            [ showString "Replace"
            , showsPrec 11 i
            , showsPrec 11 s
            , showsPrec 11 t ]

    showsPrec d NoChange        = showParen (d > 10) $ showString "NoChange"

instance Eq a => Editable [a] (Replace a) where
    apply :: Eq a => Replace a -> [a] -> Maybe [a]
    apply (Replace i s t) xs
        | i >= 0 && i <= length xs = let
            (prefix, ys) = splitAt i xs
            (ws, suffix) = splitAt (length s) ys
            in if ws == s
                then Just $ prefix <> (t <> suffix)
                else Nothing
        | otherwise               = Nothing
    apply NoChange xs             = Just xs

instance Eq a => Invertable (Replace a) where
    invert :: Eq a => Replace a -> Replace a
    invert (Replace i s t) = Replace i t s
    invert NoChange        = NoChange

instance Ord a => Rebasable (Replace a) where
    rebase :: Ord a => Replace a -> Replace a -> (Replace a, Replace a)
    rebase o1 o2 = (normalize o1', normalize o2') where
        (o1', o2')         = rebase_ (normalize o1) (normalize o2)
        rebase_ NoChange o = (NoChange, o)
        rebase_ o NoChange = (o, NoChange)
        rebase_ (Replace i1 s1 t1) (Replace i2 s2 t2) = let
            [ls1, lt1, ls2, lt2] = map length [s1, t1, s2, t2]
            j1 = i1 + ls1
            j2 = i2 + ls2
            in
                -- i1
                -- |-------------------------
                --               |-----------
                --               i2
                if i1 < i2 then
                -- i1          j1
                -- |-----------|
                --               |---------|
                --               i2        j2
                    if j1 <= i2 then
                        ( Replace i1 s1 t1
                        , Replace (i2 - ls1 + lt1) s2 t2 )
                -- i1                 j1
                -- |------------------|
                --               |---------|
                --               i2        j2
                    else if j1 <= j2 then
                        ( Replace i1 (take (i2 - i1) s1) t1
                        , Replace (i1 + lt1) (drop (j1 - i2) s2) t2 )
                -- i1                      j1
                -- |-----------------------|
                --               |-------|
                --               i2      j2
                    else
                        ( Replace i1 (splice (i2 - i1) ls2 t2 s1) t1
                        , NoChange )
                --               i1
                --               |-----------
                -- |-------------------------
                -- i2
                else if i1 > i2 then
                --               i1        j1
                --               |---------|
                -- |----------|
                -- i2         j2
                    if j2 <= i1 then
                        ( Replace (i1 - ls2 + lt2) s1 t1
                        , Replace i2 s2 t2 )
                --               i1        j1
                --               |---------|
                -- |------------------|
                -- i2                 j2
                    else if j2 <= j1 then
                        ( Replace (i2 + lt2) (drop (j2 - i1) s1) t1
                        , Replace i2 (take (i1 - i2) s2) t2 )
                --               i1      j1
                --               |-------|
                -- |-----------------------|
                -- i2                      j2
                    else
                        ( NoChange
                        , Replace i2 (splice (i1 - i2) ls1 t1 s2) t2 )
                -- i1
                -- |-------------------------
                -- |-------------------------
                -- i2
                else
                    -- i1          j1
                    -- |-----------|
                    -- |-----------------------|
                    -- i2                      j2
                    if j1 < j2 then
                        ( Replace i1 [] t1
                        , Replace (i2 + lt1) (drop ls1 s2) t2 )
                    -- i1                      j1
                    -- |-----------------------|
                    -- |-----------|
                    -- i2          j2
                    else if j1 > j2 then
                        ( Replace (i1 + lt2) (drop ls2 s1) t1
                        , Replace i2 [] t2 )
                    -- i1                      j1
                    -- |-----------------------|
                    -- |-----------------------|
                    -- i2                      j2
                    else if t1 < t2
                        then ( Replace i1 [] t1
                             , Replace (i1 + lt1) [] t2 )
                        else ( Replace (i2 + lt2) [] t1
                             , Replace i2 [] t2 )