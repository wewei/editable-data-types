{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Editable.Tree.InsDelMov where

import Data.Tree ( Tree(..), Forest )
import Editable.Core (Invertable (invert), Editable (apply, (~)), Rebasable(rebase), batchRebase)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Editable.Tree.TreeIx (TreeIx (..), root, (<~), diff, sibling, isValid)

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
    apply (Delete ixA _) ix
        | not (isValid ixA) = Nothing
        | ixA == TreeIx []  = Nothing
        | otherwise = case diff ixA ix of
            (_, TreeIx [], _) ->
                Just (TreeIx [])
            (ixBase, TreeIx [y], TreeIx (z:zs)) ->
                if y < z
                    then Just $ ixBase <> TreeIx ((z - 1):zs)
                    else Just ix
            _ -> Just ix
    apply (Move ixA ixB) ix@(TreeIx (m:ms))
        | not (isValid ixA) || not (isValid ixB)
                     = Nothing
        | ixB <~ ixA = Nothing
        | ix <~ ixA  = let
            (_, ix', _) = diff ix ixA
            in Just (ixB <> ix')
        | otherwise  = do
            let del = Delete ixA (Node () [])
            ixB' <- Just ixB ~ del
            let ins = Insert ixB' (Node () [])
            Just ix ~ del ~ ins


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

instance Ord a => Rebasable (InsDelMov a) where
    rebase :: Ord a => InsDelMov a -> InsDelMov a -> Maybe (InsDelMov a, InsDelMov a)
    rebase NoChange x = Just (NoChange, x)
    rebase o1@(Insert ns1 t1) o2@(Insert ns2 t2)
        | ns1 /= ns2  = do
            ns1' <- Just ns1 ~ o2
            ns2' <- Just ns1 ~ o1
            return (Insert ns1' t1, Insert ns2' t2)
        | t1 < t2     = Just (o2, Insert (sibling ns2) t2)
        | t1 > t2     = Just (Insert (sibling ns1) t1, o2)
        | otherwise   = Just (NoChange, NoChange)

    rebase o1@(Insert ns1 t1) o2@(Delete ns2 t2)
        | ns1 == ns2  = Just (o1, Delete (sibling ns2) t2)
        | ns1 <~ ns2  = do
            let (_, ns1', _) = diff ns1 ns2
            t2' <- joinTree ns1' t1 t2
            return (NoChange, Delete ns2 t2')
        | otherwise   = do
            ns1' <- Just ns1 ~ o2
            ns2' <- Just ns2 ~ o1
            return (Insert ns1' t1, Insert ns2' t2)
    -- TODO
    rebase o1@(Insert ns1 t1) o2@(Move ns2 ms2)
        = Just (o1, o2)
    
    rebase o1@(Delete ns1 t1) o2@(Delete ns2 t2)
        = Just (o1, o2)

    rebase o1@(Delete ns1 t1) o2@(Move ns2 ms2)
        = Just (o1, o2)
    
    rebase o1@(Move ns1 ms1) o2@(Move ns2 ms2)
        = Just (o1, o2)

    -- End TODO
    rebase o1 o2      = swap <$> rebase o2 o1