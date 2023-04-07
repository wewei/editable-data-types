{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Editable.Tree.TreeIx where
import Editable.Core (Editable)
import Data.Tree (Tree (..))
import Editable (Editable(apply))

newtype TreeIx = TreeIx [Int] deriving (Ord, Eq, Show)

root :: TreeIx
root = TreeIx []

instance Semigroup TreeIx where
    (<>) :: TreeIx -> TreeIx -> TreeIx
    TreeIx xs <> TreeIx ys = TreeIx (xs <> ys)

instance Monoid TreeIx where
    mempty :: TreeIx
    mempty = root

isValid :: TreeIx -> Bool
isValid (TreeIx xs) = all (>= 0) xs

(~>) :: TreeIx -> TreeIx -> Bool
(TreeIx xs) ~> (TreeIx ys) = take (length xs) ys == xs

(<~) :: TreeIx -> TreeIx -> Bool
(<~) = flip (~>)

diff :: TreeIx -> TreeIx -> (TreeIx, TreeIx, TreeIx)
diff (TreeIx ns) (TreeIx ms) = (TreeIx bs, TreeIx ns', TreeIx ms') where
    (bs, ns', ms') = diff_ [] ns ms
    diff_ bs [] ys = (reverse bs, [], ys)
    diff_ bs xs [] = (reverse bs, xs, [])
    diff_ bs xs@(x:xs') ys@(y:ys')
        | x == y    = diff_ (x:bs) xs' ys'
        | otherwise = (reverse bs, xs, ys)

parent :: TreeIx -> TreeIx
parent (TreeIx []) = root
parent (TreeIx ns) = TreeIx (init ns)

sibling :: TreeIx -> TreeIx
sibling (TreeIx ns) = TreeIx (next ns) where
    next ns = case reverse ns of
        x:xs -> reverse (x+1:xs)

instance Editable (Tree a) TreeIx where
    apply :: TreeIx -> Tree a -> Maybe (Tree a)
    apply (TreeIx []) t = Just t
    apply (TreeIx (n:ns)) (Node v ch)
        | n < 0     = Nothing
        | otherwise = case splitAt n ch of
            (_, c:_) -> apply (TreeIx ns) c
            _        -> Nothing


