module Main where

import Editable.List.ReplaceSegment (splice, ReplaceSegment (ReplaceSegment))
import Test.Hspec ( hspec, describe, it, shouldBe )
import Editable.Core
    ( Editable(apply),
      Rebasable((+>)),
      Invertable(invert) )
import Control.Monad (forM_, replicateM, replicateM_, when, unless, (>=>))
import System.Random ( randomRIO )
import Text.Printf ( printf )
import Control.Monad.Writer (runWriter)
import Editable.Properties (CP1Case(CP1Case), Debuggable (debug), fuzzCP1, fuzzCP2, CP2Case (CP2Case))

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

runTestCP1 :: (Eq d, Show d, Show o, Rebasable o, Editable d o) =>
           o -> o -> d -> IO ()
runTestCP1 o1 o2 d = do
    r <- debug $ CP1Case d o1 o2
    r `shouldBe` True

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
                ] $ \(o1, o2) ->
                    runTestCP1 o1 o2 "abcdefg"
            forM_
                [ (ReplaceSegment 1 "bcd" "", ReplaceSegment 2 "c" "")
                , (ReplaceSegment 1 "bcd" "x", ReplaceSegment 2 "c" "y")
                , (ReplaceSegment 1 "bcd" "xy", ReplaceSegment 5 "f" "z")
                , (ReplaceSegment 1 "bcd" "xy", ReplaceSegment 2 "cde" "z")
                ] $ \(o1, o2) ->
                    runTestCP1 o1 o2 "abcdefg"
            runTestCP1 (ReplaceSegment 0 "vq" "psj") (ReplaceSegment 0 "vqd" "o") "vqdu"
            runTestCP1 (ReplaceSegment 0 "k" "js") (ReplaceSegment 0 "k" "tdvgb") "k"

        it "should pass the CP1 fuzz test" $ do
            replicateM_ 100 $ fuzzCP1 (randStr (0, 20)) randRS >>= (`shouldBe` True)
        -- it "should pass the CP2 fuzz test" $ do
        --     forM_
        --         [ CP2Case (ReplaceSegment 3 "" "b") (ReplaceSegment 2 "" "d") (ReplaceSegment 0 "iho" "xgphg")
        --         ] $ debug >=> (`shouldBe` True)
        --     replicateM_ 100 $ fuzzCP2 (randStr (0, 20)) randRS >>= (`shouldBe` True)