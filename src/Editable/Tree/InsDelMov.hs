{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Editable.Tree.InsDelMov where

import Data.Tree ( Tree(..), Forest )
import Editable.Core (Invertable (invert), Editable (apply, (~)), Rebasable(rebase), batchRebase)
import Data.Maybe (fromJust)

newtype TreeIx = TreeIx [Int] deriving (Ord, Eq, Show)

(~>) :: TreeIx -> TreeIx -> Bool
(TreeIx ns) ~> (TreeIx ms) = take (length ms) ns == ms

parent :: TreeIx -> TreeIx
parent (TreeIx []) = TreeIx []
parent (TreeIx ns) = TreeIx . reverse . drop 1 . reverse $ ns

root :: TreeIx
root = TreeIx []

instance Editable (Tree a) TreeIx where
    apply :: TreeIx -> Tree a -> Maybe (Tree a)
    apply (TreeIx []) t = Just t
    apply (TreeIx (n:ns)) (Node v ch)
        | n < 0     = Nothing
        | otherwise = case splitAt n ch of
            (_, c:_) -> apply (TreeIx ns) c
            _        -> Nothing

data InsDelMov a
    = Insert TreeIx (Tree a)
    | Delete TreeIx (Tree a)
    | Move   TreeIx TreeIx
    | NoChange

deriving instance Eq a => Eq (InsDelMov a)

deriving instance Show a => Show (InsDelMov a)

instance Invertable (InsDelMov a) where
    invert :: InsDelMov a -> InsDelMov a
    invert NoChange      = NoChange
    invert (Insert ns t) = Delete ns t
    invert (Delete ns t) = Insert ns t
    invert (Move ns ms)  = Move ms ns

splitTree :: TreeIx -> Tree a -> Maybe (Tree a, Tree a)
splitTree (TreeIx []) _  = Nothing
splitTree (TreeIx [n]) (Node v ch)
    | n < 0             = Nothing
    | n >= length ch    = Nothing
    | otherwise         = let
        (xs, y:ys) = splitAt n ch
        in Just (Node v (xs <> ys), y)
splitTree (TreeIx (n:ns)) (Node v ch)
    | n < 0             = Nothing
    | n >= length ch    = Nothing
    | otherwise         = do
        let (xs, y:ys) = splitAt n ch
        (y', z) <- splitTree (TreeIx ns) y
        return (Node v (xs <> (y':ys)), z)

joinTree :: TreeIx -> Tree a -> Tree a -> Maybe (Tree a)
joinTree (TreeIx []) _ _ = Nothing
joinTree (TreeIx [n]) t (Node v ch)
    | n < 0         = Nothing
    | n > length ch = Nothing
    | otherwise     = let
        (xs, ys) = splitAt n ch
        in Just $ Node v (xs <> (t:ys))
joinTree (TreeIx (n:ns)) t (Node v ch)
    | n < 0         = Nothing
    | n < length ch = do
        let (xs, y:ys) = splitAt n ch
        y' <- joinTree (TreeIx ns) t y
        return $ Node v (xs <> (y':ys))
    | otherwise     = Nothing

instance Editable TreeIx (InsDelMov a) where
    apply :: InsDelMov a -> TreeIx -> Maybe TreeIx
    apply _ (TreeIx []) = Just root
    apply NoChange ix = Just ix
    apply (Insert (TreeIx []) _) _ = Nothing
    apply (Insert (TreeIx [n]) _) ix@(TreeIx (m:ms))
        | n < 0     = Nothing
        | n > m     = Just ix
        | otherwise = Just . TreeIx $ (m + 1) : ms
    apply (Insert (TreeIx (n:ns)) t) ix@(TreeIx (m:ms))
        | n < 0     = Nothing
        | n /= m    = Just ix
        | otherwise = do
            TreeIx ms' <- apply (Insert (TreeIx ns) t) (TreeIx ms)
            return . TreeIx $ n:ms'
    apply (Delete (TreeIx []) _) _ = Nothing
    apply (Delete (TreeIx [n]) _) ix@(TreeIx (m:ms))
        | n < 0     = Nothing
        | n > m     = Just ix
        | n == m    = Just root
        | otherwise = Just . TreeIx $ (m - 1) : ms
    apply (Delete (TreeIx (n:ns)) _) ix@(TreeIx (m:ms))
        | n < 0     = Nothing
        | n /= m    = Just ix
        | otherwise = do
            TreeIx ms' <- apply (Delete (TreeIx ns) (Node () [])) (TreeIx ms)
            return . TreeIx $ if null ms' then [] else n:ms'


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
    | otherwise = [n]
adjustDeletion (n:ns) (m:ms)
    | n == m    = m : adjustDeletion ns ms
    | otherwise = m : ms

next :: [Int] -> [Int]
next []     = []
next [n]    = [n + 1]
next (n:ns) = n:next ns

instance Eq a => Editable (Tree a) (InsDelMov a) where
    apply :: InsDelMov a -> Tree a -> Maybe (Tree a)
    apply NoChange s      = Just s
    apply (Insert ns t) s = joinTree ns t s
    apply (Delete ns t) s = do
        (s', t') <- splitTree ns s
        if t' == t then Just s' else Nothing
    apply (Move ns ms) s  = do
        ms' <- Just ms ~ Delete ns (Node () [])
        if ms' == root then Nothing else do
            (s', t) <- splitTree ns s
            joinTree ms' t s'

(~=) :: [Int] -> [Int] -> Bool
ns ~= ms = take (length ms) ns == ms

-- instance Ord a => Rebasable (InsDelMov a) where
--     rebase :: Ord a => InsDelMov a -> InsDelMov a -> Maybe (InsDelMov a, InsDelMov a)
--     rebase NoChange x = Just (NoChange, x)
--     rebase x NoChange = Just (x, NoChange)
--     rebase (Insert ns1 t1) (Insert ns2 t2)
--         | ns1 /= ns2  = Just
--             ( Insert (adjustInsertion ns2 ns1) t1
--             , Insert (adjustInsertion ns1 ns2) t2)
--         | t1 < t2     = Just
--             ( Insert ns1 t1
--             , Insert (next ns2) t2)
--         | t1 > t2     = Just
--             ( Insert (next ns1) t1
--             , Insert ns2 t2)
--         | otherwise   = Just (NoChange, NoChange)
--     rebase (Insert ns1 t1) (Delete ns2 t2)
--         | ns1 ~= ns2  = Just
--             ( NoChange
--             , Delete ns2 (fromJust (joinTree (drop (length ns2) ns1) t1 t2)))
--         | ns1 /= ns2  = Just
--             ( Insert (adjustDeletion ns2 ns1) t1
--             , Delete (adjustInsertion ns1 ns2) t2)
--         | otherwise   = Just
--             ( Insert ns1 t1
--             , Delete (next ns2) t2)
--     -- rebase (Insert ns1 t1) (Move ns2 ms)
--     --     | 
