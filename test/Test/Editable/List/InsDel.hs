module Test.Editable.List.InsDel where
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Editable.List.InsDel (InsDel(..))
import Editable.Core ( (~) )

testSuite :: SpecWith ()
testSuite =
    describe "InsDel" $ do
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
            
