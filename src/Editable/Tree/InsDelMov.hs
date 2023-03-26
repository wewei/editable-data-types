{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Editable.Tree.InsDelMov where

import Data.Tree ( Tree(..), Forest )
import Editable.Core (Invertable (invert), Editable (apply))

data InsDelMov a
    = Insert [Int] (Tree a)
    | Delete [Int] (Tree a)
    | Move   [Int] [Int]
    | NoChange

deriving instance Eq a => Eq (InsDelMov a)

deriving instance Show a => Show (InsDelMov a)

instance Invertable (InsDelMov a) where
    invert :: InsDelMov a -> InsDelMov a
    invert NoChange      = NoChange
    invert (Insert ns t) = Delete ns t
    invert (Delete ns t) = Insert ns t
    invert (Move ns ms)  = Move ms ns

splitTree :: [Int] -> Tree a -> Maybe (Tree a, Tree a)
splitTree [] _        = Nothing
splitTree [n] (Node root sub)
    | n < 0           = Nothing
    | n >= length sub = Nothing
    | otherwise       = let
        (xs, y:ys) = splitAt n sub
        in Just (Node root (xs <> ys), y)
splitTree (n:ns) (Node root sub)
    | n < 0           = Nothing
    | n >= length sub = Nothing
    | otherwise       = do
        let (xs, y:ys) = splitAt n sub
        (y', z) <- splitTree ns y
        return (Node root (xs <> (y':ys)), z)

joinTree :: [Int] -> Tree a -> Tree a -> Maybe (Tree a)
joinTree [] _ _ = Nothing
joinTree [n] t (Node root sub)
    | n < 0          = Nothing
    | n > length sub = Nothing
    | otherwise      = let
        (xs, ys) = splitAt n sub
        in Just $ Node root (xs <> (t:ys))
joinTree (n:ns) t (Node root sub)
    | n < 0          = Nothing
    | n < length sub = do
        let (xs, y:ys) = splitAt n sub
        y' <- joinTree ns t y
        return $ Node root (xs <> (y':ys))
    | otherwise      = Nothing

adjustInsertion :: [Int] -> [Int] -> [Int]
adjustInsertion [] ms = ms
adjustInsertion _ []  = []
adjustInsertion [n] (m:ms)
    | n <= m    = (m + 1) : ms
    | otherwise = m : ms
adjustInsertion (n:ns) (m:ms)
    | n == m    = n : adjustInsertion ns ms
    | otherwise = m : ms

adjustDeletion :: [Int] -> [Int] -> [Int]
adjustDeletion [] ms = ms
adjustDeletion _ []  = []
adjustDeletion [n] (m:ms)
    | n < m     = (m - 1) : ms
    | n > m     = m : ms
    | otherwise = []
adjustDeletion (n:ns) (m:ms)
    | n == m    = let
        ms' = adjustDeletion ns ms
        in if null ms' then [] else n:ms'
    | otherwise = m : ms


instance Eq a => Editable (Tree a) (InsDelMov a) where
    apply :: InsDelMov a -> Tree a -> Maybe (Tree a)
    apply NoChange s      = Just s
    apply (Insert ns t) s = joinTree ns t s
    apply (Delete ns t) s = do
        (s', t') <- splitTree ns s
        if t' == t then Just s' else Nothing
    apply (Move ns ms) s  = let
        ms' = adjustDeletion ns ms
        in
            if null ms' then Nothing else do
                (s', t) <- splitTree ns s
                joinTree ms' t s'
