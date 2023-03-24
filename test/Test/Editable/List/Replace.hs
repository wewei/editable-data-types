module Test.Editable.List.Replace where

import Editable.List.Replace (Replace (Replace, NoChange), normalize)
import Test.Hspec ( hspec, describe, it, shouldBe )
import Editable.Core
    ( Editable(apply),
      Rebasable((+>)),
      Invertable(invert) )
import Control.Monad (forM_, replicateM, replicateM_, when, unless, (>=>))
import System.Random ( randomRIO )
import Control.Monad.Writer (runWriter)
import Test.Hspec.Runner (SpecWith)
import Test.Util (replM, generateOps)
import Editable.Properties
    ( CP1Case(CP1Case), Testable(test), fuzzCP1 )

randStr :: (Int, Int) -> IO String
randStr range = replM (randomRIO range) (randomRIO ('a', 'z'))

randDoc :: IO String
randDoc = randStr (0, 8)

randReplace :: [Char] -> IO (Replace Char)
randReplace str = do
    let len = length str
    i       <- randomRIO (0, len)
    l       <- randomRIO (0, len - i)
    let src = take l . drop i $ str
    tar     <- randStr (0, 5)
    return . normalize $ Replace i src tar

testSuite :: SpecWith ()
testSuite = describe "Replace" $ do
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
            , CP1Case "lceoly" (Replace 1 "c" "ywrtc") (Replace 3 "" "yhu")
            ] $ (`shouldBe` True) . test
        -- forM_ [ ] $ debug >=> (`shouldBe` True)

    it "should pass the CP1 fuzz test" $ do
        replicateM_ 5000 $
            fuzzCP1 randDoc randReplace >>= (`shouldBe` True)

    it "should pass the batch CP1 fuzz test" $ do
        replicateM_ 5000 $
            fuzzCP1 randDoc (generateOps (randomRIO (2, 5)) randReplace) >>= (`shouldBe` True)
