module Main where

import Editable.List.Replace (splice, Replace (Replace))
import Test.Hspec ( hspec, describe, it, shouldBe )
import Editable.Core
    ( Editable(apply),
      Rebasable((+>)),
      Invertable(invert) )
import Control.Monad (forM_, replicateM, replicateM_, when, unless, (>=>))
import System.Random ( randomRIO )
import Text.Printf ( printf )
import Control.Monad.Writer (runWriter)
import Editable.Properties (CP1Case(CP1Case), Debuggable (debug), fuzzCP1, fuzzCP2, CP2Case (CP2Case), Testable (test))

randStr :: (Int, Int) -> IO [Char]
randStr range = do
    len <- randomRIO range
    replicateM len . randomRIO $ ('a', 'z')

randRS :: [Char] -> IO (Replace Char)
randRS str = do
    let len = length str
    i       <- randomRIO (0, len)
    l       <- randomRIO (0, len - i)
    let src = drop i . take l $ str
    tar     <- randStr (0, 5)
    return $ Replace i src tar

main :: IO ()
main = hspec $ do
    describe "splice" $ do
        it "should remove a specified segment" $ do
            splice 1 3 "" "abcdefg" `shouldBe` "aefg"
        it "should insert the specifed segment" $ do
            splice 1 0 "xyz" "abcdefg" `shouldBe` "axyzbcdefg"
        it "should replace the specifed segment" $ do
            splice 1 3 "xyz" "abcdefg" `shouldBe` "axyzefg"
    describe "Replace" $ do
        it "should remove a specified segment" $ do
            apply (Replace 1 "bcd" "") "abcdefg" `shouldBe` Just "aefg"
        it "should insert the specifed segment" $ do
            apply (Replace 1 "" "xyz") "abcdefg" `shouldBe` Just "axyzbcdefg"
        it "should replace the specifed segment" $ do
            apply (Replace 1 "bcd" "xyz") "abcdefg" `shouldBe` Just "axyzefg"
        it "should confirm to CP1" $ do
            forM_
                [ CP1Case "abcdefg" (Replace 1 "bcd" "") (Replace 2 "c" "")
                , CP1Case "abcdefg" (Replace 1 "bcd" "x") (Replace 2 "c" "y")
                , CP1Case "abcdefg" (Replace 1 "bcd" "xy") (Replace 5 "f" "z")
                , CP1Case "abcdefg" (Replace 1 "bcd" "xy") (Replace 2 "cde" "z")
                , CP1Case "vqdu" (Replace 0 "vq" "psj") (Replace 0 "vqd" "o") 
                , CP1Case "k" (Replace 0 "k" "js") (Replace 0 "k" "tdvgb")
                ] $ (`shouldBe` True) . test

        it "should pass the CP1 fuzz test" $ do
            replicateM_ 100 $ fuzzCP1 (randStr (0, 20)) randRS >>= (`shouldBe` True)
        it "should pass the CP2 fuzz test" $ do
            forM_
                [ CP2Case (Replace 3 "" "b") (Replace 2 "" "d") (Replace 0 "iho" "xgphg")
                ] $ debug >=> (`shouldBe` True)
            replicateM_ 100 $ fuzzCP2 (randStr (0, 20)) randRS >>= (`shouldBe` True)