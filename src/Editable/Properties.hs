{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE InstanceSigs #-}
module Editable.Properties where
import Editable.Core ( Editable((~)), Rebasable (rebase, (+>)), Rebase (Rebase))
import Control.Monad ( foldM, forM_, unless )
import Control.Monad.Writer.Lazy ( Writer, MonadWriter (tell) )
import Text.Printf (printf)
import Data.List (intersperse)

class Testable t where
    test :: t -> Bool

class Testable t => Debuggable t where 
    debug :: t -> IO Bool
    debug = return . test 

-- Testing CP1
data (Eq d, Rebasable o, Editable d o) => CP1Case o d = CP1Case d o o

instance (Eq d, Show d, Show o, Rebasable o, Editable d o) => Show (CP1Case o d) where
    showsPrec :: (Eq d, Show d, Show o, Rebasable o, Editable d o) => Int -> CP1Case o d -> ShowS
    showsPrec d (CP1Case o o1 o2) = showParen (d > 10) $
        foldl1 (.) $ intersperse (showChar ' ')
            [ showString "CP1Case"
            , showsPrec 11 o
            , showsPrec 11 o1
            , showsPrec 11 o2 ]

instance (Eq d, Rebasable o, Editable d o) => Testable (CP1Case o d) where
    test :: (Eq d, Rebasable o, Editable d o) => CP1Case o d -> Bool
    test (CP1Case b o1 o2) = Just b ~ o1 ~ o2' == Just b ~ o2 ~ o1' where
        (o1', o2') = rebase o1 o2

instance (Eq d, Show d, Show o, Rebasable o, Editable d o) => Debuggable (CP1Case o d) where
     debug :: (Eq d, Show d, Show o, Rebasable o, Editable d o) => CP1Case o d -> IO Bool
     debug (CP1Case b o1 o2) = do
        let (o1', o2') = rebase o1 o2
            m1         = Just b ~ o1
            d1         = m1 ~ o2'
            m2         = Just b ~ o2
            d2         = m2 ~ o1'
        printf "Base:\n"
        printf "    %s\n" $ show b
        printf "Path 1:\n"
        printf "  ~ %s\n" $ show o1
        printf " -> %s\n" $ show m1
        printf "  ~ %s\n" $ show o2'
        printf " -> %s\n" $ show d1
        printf "Path 2:\n"
        printf "  ~ %s\n" $ show o2
        printf " -> %s\n" $ show m2
        printf "  ~ %s\n" $ show o1'
        printf " -> %s\n" $ show d2
        printf "\n"
        return (d1 == d2)

fuzzCP1 :: (Eq d, Show d, Show o, Editable d o, Rebasable o) => IO d -> (d -> IO o) -> IO Bool
fuzzCP1 genDoc genOp = do
    d  <- genDoc
    o1 <- genOp d
    o2 <- genOp d
    let c = CP1Case d o1 o2
        r = test c
    unless r $ do
        putStrLn "Failed case:"
        print c
    return r

data (Eq o, Rebasable o) => CP2Case o = CP2Case o o o

instance (Eq o, Rebasable o, Show o) => Show (CP2Case o) where
    showsPrec :: (Eq o, Rebasable o, Show o) => Int -> CP2Case o -> ShowS
    showsPrec d (CP2Case o o1 o2) = showParen (d > 10) $
        foldl1 (.) $ intersperse (showChar ' ') [showString "CP2Case", showsPrec 11 o, showsPrec 11 o1, showsPrec 11 o2]

instance (Eq o, Rebasable o) => Testable (CP2Case o) where
    test :: (Eq o, Rebasable o) => CP2Case o -> Bool
    test (CP2Case o o1 o2) = test (CP1Case o (Rebase o1) (Rebase o2))

instance (Eq o, Rebasable o, Show o) => Debuggable (CP2Case o) where
    debug :: (Eq o, Rebasable o, Show o) => CP2Case o -> IO Bool
    debug (CP2Case o o1 o2) = debug (CP1Case o (Rebase o1) (Rebase o2))

fuzzCP2 :: (Eq d, Eq o, Show d, Show o, Editable d o, Rebasable o) => IO d -> (d -> IO o) -> IO Bool
fuzzCP2 genDoc genOp = do
    d  <- genDoc
    o  <- genOp d
    o1 <- genOp d
    o2 <- genOp d
    let c = CP2Case o o1 o2
        r = test c
    unless r $ do
        putStrLn "Failed case:"
        print c
    return r