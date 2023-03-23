module Editable.List.Util where

splice :: Int -> Int -> [a] -> [a] -> [a]
splice i l xs ys = let
    (prefix, zs) = splitAt i ys
    (ws, suffix) = splitAt l zs
    in prefix <> (xs <> suffix)

