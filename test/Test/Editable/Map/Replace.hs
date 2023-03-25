module Test.Editable.Map.Replace where
import Test.Hspec (describe, it, shouldBe, SpecWith)
import Editable.Core ((~), Invertable (invert))
import Editable.Map.Replace (Replace(..))
import Editable.Properties (fuzzCP1, fuzzCP2)
import Control.Monad (replicateM_, replicateM)
import Test.Util (replM, generateOps)
import System.Random ( randomRIO )
import Data.Map (Map, fromList, assocs, singleton)

randStr :: (Int, Int) -> IO String
randStr range = replM (randomRIO range) (randomRIO ('a', 'z'))

randDoc :: IO (Map String String)
randDoc = do
    n    <- randomRIO (0, 8)
    ascs <- replicateM n $ do
        k <- randStr (3, 8)
        v <- randStr (3, 8)
        return (k, v)
    return $ fromList ascs

randReplace :: Map String String -> IO (Replace String String)
randReplace d = do
    let ascs   = assocs d
        len    = length ascs
    t <- randomRIO (0, 4 :: Int)
    if t == 0 || len == 0 then do
        k <- randStr (3, 8)
        u <- randStr (3, 8)
        return $ Replace k Nothing (Just u)
    else if t == 1 then do
        i <- randomRIO (0, len - 1)
        let (k, v) = ascs !! i
        return $ Replace k (Just v) Nothing
    else do
        i <- randomRIO (0, len - 1)
        let (k, v) = ascs !! i
        u <- randStr (3, 8)
        return $ Replace k (Just v) (Just u)

testSuite :: SpecWith ()
testSuite =
    describe "Replace" $ do
        describe "as Editable" $ do
            describe "Replace" $ do
                it "should change the value bind to the given key" $ do
                    Just (fromList [("Foo", "tic"), ("Bar", "tac")])
                        ~          Replace "Foo" (Just "tic") (Just "toe")
                        `shouldBe` Just (fromList [("Foo", "toe"), ("Bar", "tac")])
                    Just (fromList [("Foo", "tic"), ("Bar", "tac")])
                        ~          Replace "Foo" (Just "tic") Nothing
                        `shouldBe` Just (singleton "Bar" "tac")
                    Just (singleton "Foo" "tic")
                        ~          Replace "Bar" Nothing (Just "tac")
                        `shouldBe` Just (fromList [("Foo", "tic"), ("Bar", "tac")])

                it "should fail if the original value doesn not match" $ do
                    Just (fromList [("Foo", "tic"), ("Bar", "tac")])
                        ~          Replace "Foo" (Just "Tic") (Just "toe")
                        `shouldBe` Nothing

            describe "NoChange" $ do
                it "should keep the value unchanged" $ do
                    Just (fromList [("Foo", "tic"), ("Bar", "tac")])
                        ~          (NoChange :: Replace String String)
                        `shouldBe` Just (fromList [("Foo", "tic"), ("Bar", "tac")])
        describe "as Invertable" $ do
            it "should invert the operations" $ do
                invert (Replace "Foo" Nothing (Just "tic"))
                    `shouldBe` Replace "Foo" (Just "tic") Nothing
                invert (Replace "Foo" (Just "tic") Nothing)
                    `shouldBe` Replace "Foo" Nothing (Just "tic")
                invert (Replace "Foo" (Just "tic") (Just "tac"))
                    `shouldBe` Replace "Foo" (Just "tac") (Just "tic")
        describe "as Rebasable" $ do
            it "should pass the CP1 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP1 randDoc randReplace >>= (`shouldBe` True)
            it "should pass the batch CP1 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP1 randDoc (generateOps (randomRIO (2, 5)) randReplace) >>= (`shouldBe` True)
            it "should pass the CP2 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP2 randDoc randReplace >>= (`shouldBe` True)
            it "should pass the batch CP2 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP2 randDoc (generateOps (randomRIO (2, 5)) randReplace) >>= (`shouldBe` True)

