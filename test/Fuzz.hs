module Fuzz where
import Editable.Core ( Rebasable, Editable )
import Control.Monad.Writer ( unless, runWriter )
import Editable.Properties ( testCP1, testCP2 )
import Text.Printf ( printf )
import Test.Hspec ( shouldBe )

fuzzCP1 :: (Eq d, Show d, Show o, Editable d o, Rebasable o) => IO d -> (d -> IO o) -> IO ()
fuzzCP1 genDoc genOp = do
    d  <- genDoc
    o1 <- genOp d
    o2 <- genOp d
    let (result, log) = runWriter $ testCP1 o1 o2 d
    putStrLn . log $ ""
    unless result $ do
        printf "Rerun failed case:\n\trunWriter $ testCP1 (%s) (%s) (%s)\n" (show o1) (show o2) (show d)
    result `shouldBe` True

fuzzCP2 :: (Eq d, Eq o, Show d, Show o, Editable d o, Rebasable o) => IO d -> (d -> IO o) -> IO ()
fuzzCP2 genDoc genOp = do
    d  <- genDoc
    o1 <- genOp d
    o2 <- genOp d
    t  <- genOp d
    let (result, log) = runWriter $ testCP2 o1 o2 t
    putStrLn . log $ ""
    unless result $ do
        printf "Rerun failed case:\n\trunWriter $ testCP2 (%s) (%s) (%s)\n" (show o1) (show o2) (show t)
    result `shouldBe` True

