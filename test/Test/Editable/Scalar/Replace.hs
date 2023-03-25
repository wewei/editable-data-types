module Test.Editable.Scalar.Replace where
import Test.Hspec (describe, it, shouldBe, SpecWith)
import Editable.Core ((~), Invertable (invert))
import Editable.Scalar.Replace (Replace(..))
import Editable.Properties (fuzzCP1, fuzzCP2)
import Control.Monad (replicateM_)
import Test.Util (replM, generateOps)
import System.Random ( randomRIO )

randStr :: (Int, Int) -> IO String
randStr range = replM (randomRIO range) (randomRIO ('a', 'z'))

randDoc :: IO String
randDoc = randStr (0, 8)

randReplace :: String -> IO (Replace String)
randReplace d = Replace d <$> randDoc

testSuite :: SpecWith ()
testSuite =
    describe "Replace" $ do
        describe "as Editable" $ do
            describe "Replace" $ do
                it "should change the value to the target" $ do
                    Just "Foo" ~ Replace "Foo" "Bar" `shouldBe` Just "Bar"
                it "should fail if the original value doesn not match" $ do
                    Just "Foo" ~ Replace "foo" "Bar" `shouldBe` Nothing
            describe "NoChange" $ do
                it "should keep the value unchanged" $ do
                    Just "Foo" ~ (NoChange :: Replace String) `shouldBe` Just "Foo"
        describe "as Invertable" $ do
            it "should invert the operations" $ do
                invert (Replace "Foo" "Bar") `shouldBe` Replace "Bar" "Foo"
        describe "as Rebasable" $ do
            it "should pass the CP1 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP1 randDoc randReplace >>= (`shouldBe` True)
            it "should pass the batch CP1 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP1 randDoc (generateOps (randomRIO (2, 5)) randReplace) >>= (`shouldBe` True)
                replicateM_ 100 $
                    fuzzCP1 randDoc (generateOps (randomRIO (30, 50)) randReplace) >>= (`shouldBe` True)
            it "should pass the CP2 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP2 randDoc randReplace >>= (`shouldBe` True)
            it "should pass the batch CP2 fuzz test" $ do
                replicateM_ 500 $
                    fuzzCP2 randDoc (generateOps (randomRIO (2, 5)) randReplace) >>= (`shouldBe` True)
                replicateM_ 100 $
                    fuzzCP2 randDoc (generateOps (randomRIO (30, 50)) randReplace) >>= (`shouldBe` True)
