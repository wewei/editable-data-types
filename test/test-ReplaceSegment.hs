module Main where

import Data.ReplaceSegment (splice, ReplaceSegment (ReplaceSegment))
import Test.Hspec ( hspec, describe, it, shouldBe )
import Data.Editable ( Editable(apply) )
import Data.Rebasable (checkCP1)
import Control.Monad (forM_)
import Data.Invertable (Invertable(invert))

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
                    print ds
                    result `shouldBe` True
            forM_
                [ (ReplaceSegment 1 "bcd" "", ReplaceSegment 2 "c" "")
                , (ReplaceSegment 1 "bcd" "x", ReplaceSegment 2 "c" "y")
                , (ReplaceSegment 1 "bcd" "xy", ReplaceSegment 5 "f" "z")
                , (ReplaceSegment 1 "bcd" "xy", ReplaceSegment 2 "cde" "z")
                ] $ \(o1, o2) -> do
                    let (result, ds) = checkCP1 [o1, invert o1] [o2] "abcdefg"
                    print ds
                    result `shouldBe` True