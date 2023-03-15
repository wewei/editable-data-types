module Main where

import Data.ReplaceSegment (splice, ReplaceSegment (ReplaceSegment))
import Test.Hspec ( hspec, describe, it, shouldBe )
import Data.Editable ( Editable(apply) )
import Data.Rebasable ( Rebasable, checkCP1)
import Control.Monad (forM_, replicateM, replicateM_, when, unless)
import Data.Invertable (Invertable(invert))
import System.Random ( randomRIO )
import Text.Printf ( printf )

fuzzCP1 :: (Eq d, Show d, Show o, Editable d o, Rebasable o) => IO d -> (d -> IO o) -> IO ()
fuzzCP1 genDoc genOp = do
    d <- genDoc
    o1 <- genOp d
    o2 <- genOp d
    let (result, ds) = checkCP1 o1 o2 d
    unless result $ do
        print (d, o1, o2, ds)
        printf "fst (checkCP1 (%s) (%s) %s) `shouldBe` True" (show o1) (show o2) (show d)
    result `shouldBe` True

randStr :: (Int, Int) -> IO [Char]
randStr range = do
    len <- randomRIO range
    replicateM len . randomRIO $ ('a', 'z')

randRS :: [Char] -> IO (ReplaceSegment Char)
randRS str = do
    let len = length str
    i       <- randomRIO (0, len)
    l       <- randomRIO (0, len - i)
    let src = drop i . take l $ str
    tar     <- randStr (0, 5)
    return $ ReplaceSegment i src tar

    


main :: IO ()
main = hspec $ do
    describe "splice" $ do
        it "should remove a specified segment" $ do
            splice 1 3 "" "abcdefg" `shouldBe` "aefg"
        it "should insert the specifed segment" $ do
            splice 1 0 "xyz" "abcdefg" `shouldBe` "axyzbcdefg"
        it "should replace the specifed segment" $ do
            splice 1 3 "xyz" "abcdefg" `shouldBe` "axyzefg"
    describe "ReplaceSegment" $ do
        it "should remove a specified segment" $ do
            apply (ReplaceSegment 1 "bcd" "") "abcdefg" `shouldBe` Just "aefg"
        it "should insert the specifed segment" $ do
            apply (ReplaceSegment 1 "" "xyz") "abcdefg" `shouldBe` Just "axyzbcdefg"
        it "should replace the specifed segment" $ do
            apply (ReplaceSegment 1 "bcd" "xyz") "abcdefg" `shouldBe` Just "axyzefg"
        it "should confirm to CP1" $ do
            forM_
                [ (ReplaceSegment 1 "bcd" "", ReplaceSegment 2 "c" "")
                , (ReplaceSegment 1 "bcd" "x", ReplaceSegment 2 "c" "y")
                , (ReplaceSegment 1 "bcd" "xy", ReplaceSegment 5 "f" "z")
                , (ReplaceSegment 1 "bcd" "xy", ReplaceSegment 2 "cde" "z")
                ] $ \(o1, o2) -> do
                    let (result, ds) = checkCP1 o1 o2 "abcdefg"
                    result `shouldBe` True
            forM_
                [ (ReplaceSegment 1 "bcd" "", ReplaceSegment 2 "c" "")
                , (ReplaceSegment 1 "bcd" "x", ReplaceSegment 2 "c" "y")
                , (ReplaceSegment 1 "bcd" "xy", ReplaceSegment 5 "f" "z")
                , (ReplaceSegment 1 "bcd" "xy", ReplaceSegment 2 "cde" "z")
                ] $ \(o1, o2) -> do
                    let (result, ds) = checkCP1 [o1, invert o1] [o2] "abcdefg"
                    result `shouldBe` True
            fst (checkCP1 (ReplaceSegment 0 "vq" "psj") (ReplaceSegment 0 "vqd" "o") "vqdu") `shouldBe` True
            fst (checkCP1 (ReplaceSegment 0 "k" "js") (ReplaceSegment 0 "k" "tdvgb") "k") `shouldBe` True
        it "should pass the CP1 fuzz test" $ do
            replicateM_ 100 $ fuzzCP1 (randStr (0, 20)) randRS