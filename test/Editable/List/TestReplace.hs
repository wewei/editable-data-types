{-# LANGUAGE LambdaCase #-}
module Editable.List.TestReplace where

import Editable.List.Replace (Replace (Replace, NoChange), normalize)
import Test.Hspec ( hspec, describe, it, shouldBe )
import Editable.Core
    ( Editable(apply),
      Rebasable((+>)),
      Invertable(invert) )
import Control.Monad (forM_, replicateM, replicateM_, when, unless, (>=>))
import System.Random ( randomRIO )
import Text.Printf ( printf )
import Control.Monad.Writer (runWriter)
import Editable.Properties (CP1Case(CP1Case), Debuggable (debug), fuzzCP1, fuzzCP2, CP2Case (CP2Case), Testable (test), generateCP1Case)
import Test.Hspec.Runner (SpecWith)

randStr :: (Int, Int) -> IO [Char]
randStr range = do
    len <- randomRIO range
    replicateM len . randomRIO $ ('a', 'z')

randReplace :: [Char] -> IO (Replace Char)
randReplace str = do
    let len = length str
    i       <- randomRIO (0, len)
    l       <- randomRIO (0, len - i)
    let src = drop i . take l $ str
    tar     <- randStr (0, 5)
    return . normalize $ Replace i src tar

iterM :: Monad m => Int -> a -> (a -> m (a, b)) -> m [b]
iterM = iterM_ (return []) where
    iterM_ :: Monad m => m [b] -> Int -> a -> (a -> m (a, b)) -> m [b]
    iterM_ ys 0 _ _ = reverse <$> ys
    iterM_ ys n x f = do
        (x', y) <- f x
        ys'     <- ys
        iterM_ (return (y:ys')) (n - 1) x' f

randReplaces :: (Int, Int) -> [Char] -> IO [Replace Char]
randReplaces range str = do
    len <- randomRIO range
    iterM len (Just str) $ \case
        Nothing -> return (Nothing, NoChange)
        Just s  -> do
            r <- randReplace s
            return (apply r s, r)

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
        replicateM_ 500 $ fuzzCP1 (randStr (0, 8)) randReplace >>= (`shouldBe` True)

    it "should pass the batch CP1 fuzz test" $ do
        replicateM_ 500 $ fuzzCP1 (randStr (0, 8)) (randReplaces (2, 5)) >>= (`shouldBe` True)
        generateCP1Case (randStr (0, 8)) (randReplaces (2, 5)) >>= debug >>= (`shouldBe` True)

    -- it "should confirm to CP2" $ do
    --     forM_
    --         [ CP2Case (Replace 3 "" "b") (Replace 2 "" "d") (Replace 0 "iho" "xgphg")
    --         , CP2Case (Replace 4 "" "") (Replace 4 "" "") (Replace 1 "e" "ghkdy")
    --         , CP2Case (Replace 3 "yxdbjzasdue" "") (Replace 5 "dbjzas" "kdcc") (Replace 3 "yxdbjzas" "xkhr")
    --         , CP2Case (Replace 5 "" "zjq") (Replace 5 "" "") (Replace 4 "" "mqlqt")
    --         , CP2Case (Replace 3 "ykulpcm" "q") (Replace 10 "" "p") (Replace 3 "yku" "oerk")
    --         ] $ (`shouldBe` True) . test

    --     forM_ [ CP2Case (Replace 0 "f" "t") (Replace 0 "f" "qg") (Replace 1 "" "mv") ] $ debug >=> (`shouldBe` True)

    -- it "should pass the CP2 fuzz test" $ do
    --     replicateM_ 500 $ fuzzCP2 (randStr (0, 88)) randRS >>= (`shouldBe` True)