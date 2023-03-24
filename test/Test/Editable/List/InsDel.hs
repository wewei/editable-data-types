module Test.Editable.List.InsDel where
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Editable.List.InsDel (InsDel(..))
import Editable.Core ( (~), Invertable (invert) )
import Control.Monad (replicateM_)
import Editable.Properties (fuzzCP1, generateCP1Case, Debuggable (debug), generateCP2Case, fuzzCP2)
import Test.Util (replM, generateOps)
import System.Random (randomRIO)

randStr :: (Int, Int) -> IO String
randStr range = replM (randomRIO range) (randomRIO ('a', 'z'))

randDoc :: IO String
randDoc = randStr (0, 8)

randInsDel :: String -> IO (InsDel Char)
randInsDel doc = do
    let len = length doc
    t      <- randomRIO (0, 20::Int)
    if t < 10 || len == 0 then do
        n  <- randomRIO (0, len)
        xs <- randStr (1, 3)
        return $ Insert n xs
    else if t < 20 then do
        n <- randomRIO (0, len - 1)
        l <- randomRIO (1, len - n)
        let xs = take l . drop n $ doc
        return $ Delete n xs
    else do
        return NoChange

testSuite :: SpecWith ()
testSuite =
    describe "InsDel" $ do
        describe "as Editable" $ do
            describe "Insert" $ do
                it "should insert a sequence into the target" $ do
                    Just "Bar" ~ Insert 0 "Foo" `shouldBe` Just "FooBar"
                    Just "Bar" ~ Insert 2 "Foo" `shouldBe` Just "BaFoor"
                    Just "Bar" ~ Insert 3 "Foo" `shouldBe` Just "BarFoo"
                it "should fail if the insert location is illegal" $ do
                    Just "Bar" ~ Insert (-1) "Foo" `shouldBe` Nothing
                    Just "Bar" ~ Insert 4    "Foo" `shouldBe` Nothing
            describe "Delete" $ do
                it "should remove a sequence from the target" $ do
                    Just "FooBar" ~ Delete 0 "Foo" `shouldBe` Just "Bar"
                    Just "FooBar" ~ Delete 2 "oBa" `shouldBe` Just "For"
                    Just "FooBar" ~ Delete 3 "Bar" `shouldBe` Just "Foo"
                it "should fail if the delete location is illegal" $ do
                    Just "FooBar" ~ Delete (-1) "Foo" `shouldBe` Nothing
                    Just "FooBar" ~ Delete 4    "Foo" `shouldBe` Nothing
                it "should fail if the subsequences does not match"$ do
                    Just "FooBar" ~ Delete 0 "foo" `shouldBe` Nothing
            describe "NoChange" $ do
                it "should keep the original sequence unchanged" $ do
                    Just "FooBar" ~ (NoChange :: InsDel Char) `shouldBe` Just "FooBar"
                    Just ""       ~ (NoChange :: InsDel Char) `shouldBe` Just ""
        describe "as Invertable" $ do
            it "should invert the operation" $ do
                invert (Insert 0 "FooBar") `shouldBe` Delete 0 "FooBar"
                invert (Delete 0 "FooBar") `shouldBe` Insert 0 "FooBar"
                invert (NoChange::InsDel Char) `shouldBe` NoChange
        describe "as Rebasable" $ do
            it "should pass the CP1 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP1 randDoc randInsDel >>= (`shouldBe` True)
                generateCP1Case randDoc randInsDel >>= debug >>= (`shouldBe` True)
            it "should pass the batch CP1 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP1 randDoc (generateOps (randomRIO (1, 5)) randInsDel) >>= (`shouldBe` True)
            it "should pass the CP2 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP2 randDoc randInsDel >>= (`shouldBe` True)
                generateCP2Case randDoc randInsDel >>= debug >>= (`shouldBe` True)
            it "should pass the batch CP2 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP2 randDoc (generateOps (randomRIO (1, 5)) randInsDel) >>= (`shouldBe` True)
            
