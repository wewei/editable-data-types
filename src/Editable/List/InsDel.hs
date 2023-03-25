{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
module Editable.List.InsDel where

import Editable.Core ( Invertable(invert), Editable (apply), Rebasable (rebase) )
import Numeric (showInt, showSigned)
import Data.List (intersperse)
import Editable.List.Util (splice)

data InsDel a
    = Insert Int [a]
    | Delete Int [a]
    | NoChange

normalize :: InsDel a -> InsDel a
normalize (Insert _ []) = NoChange
normalize (Delete _ []) = NoChange
normalize o             = o

instance Invertable (InsDel a) where
    invert :: InsDel a -> InsDel a
    invert (Insert n xs) = Delete n xs
    invert (Delete n xs) = Insert n xs
    invert NoChange      = NoChange

deriving instance Eq a => Eq (InsDel a)

deriving instance Show a => Show (InsDel a)

instance Eq a => Editable [a] (InsDel a) where
    apply :: Eq a => InsDel a -> [a] -> Maybe [a]
    apply NoChange xs   = Just xs
    apply (Insert n xs) ys
        | n < 0         = Nothing
        | n > length ys = Nothing
        | otherwise     = let
            (zs, ws) = splitAt n ys
            in Just $ zs <> (xs <> ws)
    apply (Delete n xs) ys
        | n < 0         = Nothing
        | n > length ys = Nothing
        | otherwise     = let
            (zs, ws) = splitAt n ys
            (us, vs) = splitAt (length xs) ws
            in if us == xs
                then Just $ zs <> vs
                else Nothing

instance Ord a => Rebasable (InsDel a) where
    rebase :: Ord a => InsDel a -> InsDel a -> (InsDel a, InsDel a)
    rebase o1 o2 = (normalize o1', normalize o2') where
        (o1', o2') = rebase_ o1 o2
        rebase_ NoChange o = (NoChange, o)
        rebase_ o NoChange = (o, NoChange)
        rebase_ (Insert n1 s1) (Insert n2 s2)
            | n1 < n2   = (Insert n1 s1, Insert (n2 + length s1) s2)
            | n1 > n2   = (Insert (n1 + length s2) s1, Insert n2 s2)
            | s1 < s2   = (Insert n1 s1, Insert (n2 + length s1) s2)
            | s2 < s1   = (Insert (n1 + length s2) s1, Insert n2 s2)
            | otherwise = (NoChange, NoChange)
        rebase_ (Delete n1 s1) (Delete n2 s2) = let
            [l1, l2] = map length [s1, s2]
            (m1, m2) = (n1 + l1, n2 + l2)
            in
                if n1 <= n2 then
                    if m1 <= n2 then
                        ( Delete n1 s1
                        , Delete (n2 - l1) s2 )
                    else if m1 <= m2 then
                        ( Delete n1 (take (n2 - n1) s1)
                        , Delete n1 (drop (m1 - n2) s2) )
                    else
                        ( Delete n1 (splice (n2 - n1) l2 [] s1)
                        , NoChange )
                else if n1 <= m2 then
                    if m1 <= m2 then
                        ( NoChange
                        , Delete n2 (splice (n1 - n2) l1 [] s2 ) )
                    else
                        ( Delete n2 (drop (m2 - n1) s1)
                        , Delete n2 (take (n1 - n2) s2) )
                else
                    ( Delete (n1 - l2) s1
                    , Delete n2 s2 )

        rebase_ (Insert n1 s1) (Delete n2 s2) = let
            [l1, l2] = map length [s1, s2]
            in
                if n1 < n2 then
                    ( Insert n1 s1
                    , Delete (n2 + l1) s2 )
                else if n1 > n2 + l2 then
                    ( Insert (n1 - l2) s1
                    , Delete n2 s2 )
                else
                    ( NoChange
                    , Delete n2 (splice (n1 - n2) 0 s1 s2) )

        rebase_ o1 o2 = let (o1', o2') = rebase_ o2 o1 in (o2', o1')