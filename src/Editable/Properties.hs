{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Editable.Properties where
import Editable.Core ( Editable((~)), Rebasable (rebase, (+>)), Rebase (Rebase))
import Control.Monad ( foldM, forM_, unless )
import Control.Monad.Writer.Lazy ( Writer, MonadWriter (tell) )
import Text.Printf (printf)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)

class Testable t where
    test :: t -> Bool

class Testable t => Debuggable t where 
    debug :: t -> IO Bool
    debug = return . test 

-- Testing CP1
data CP1Case o d = CP1Case d o o

deriving instance (Show d, Show o) => Show (CP1Case o d)

instance (Eq d, Rebasable o, Editable d o) => Testable (CP1Case o d) where
    test :: (Eq d, Rebasable o, Editable d o) => CP1Case o d -> Bool
    test (CP1Case b o1 o2) = fromMaybe False $ do
        (o1', o2') <- rebase o1 o2
        return (Just b ~ o1 ~ o2' == Just b ~ o2 ~ o1')

instance (Eq d, Show d, Show o, Rebasable o, Editable d o) => Debuggable (CP1Case o d) where
     debug :: (Eq d, Show d, Show o, Rebasable o, Editable d o) => CP1Case o d -> IO Bool
     debug (CP1Case b o1 o2) = fromMaybe (return False) $ do
        (o1', o2') <- rebase o1 o2
        return $ do
            let m1         = Just b ~ o1
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

data CP2Case o = CP2Case o o o

deriving instance Show o => Show (CP2Case o)

instance (Eq o, Rebasable o) => Testable (CP2Case o) where
    test :: (Eq o, Rebasable o) => CP2Case o -> Bool
    test (CP2Case o o1 o2) = test (CP1Case o (Rebase o1) (Rebase o2))

instance (Eq o, Rebasable o, Show o) => Debuggable (CP2Case o) where
    debug :: (Eq o, Rebasable o, Show o) => CP2Case o -> IO Bool
    debug (CP2Case o o1 o2) = debug (CP1Case o (Rebase o1) (Rebase o2))

generateCP1Case :: (Eq d, Show d, Show o, Editable d o, Rebasable o) => IO d -> (d -> IO o) -> IO (CP1Case o d)
generateCP1Case genDoc genOp = do
    d  <- genDoc
    o1 <- genOp d
    o2 <- genOp d
    return $ CP1Case d o1 o2

generateCP2Case :: (Eq d, Eq o, Show d, Show o, Editable d o, Rebasable o) => IO d -> (d -> IO o) -> IO (CP2Case o)
generateCP2Case genDoc genOp = do
    d  <- genDoc
    o  <- genOp d
    o1 <- genOp d
    o2 <- genOp d
    return $ CP2Case o o1 o2

testAndLog :: (Testable t, Show t) => t -> IO Bool
testAndLog c = do
    let r = test c
    unless r $ do
        putStrLn "Failed case:"
        print c
    return r

fuzzCP1 :: (Eq d, Eq o, Show d, Show o, Editable d o, Rebasable o) => IO d -> (d -> IO o) -> IO Bool
fuzzCP1 genDoc genOp = generateCP1Case genDoc genOp >>= testAndLog

fuzzCP2 :: (Eq d, Eq o, Show d, Show o, Editable d o, Rebasable o) => IO d -> (d -> IO o) -> IO Bool
fuzzCP2 genDoc genOp = generateCP2Case genDoc genOp >>= testAndLog
