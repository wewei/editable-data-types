module Test.Editable.List.Util where
import Test.Hspec (describe, it, shouldBe, SpecWith)
import Editable.List.Util (splice)

testSuite :: SpecWith ()
testSuite = describe "splice" $ do
    it "should remove a specified segment" $ do
        splice 1 3 "" "abcdefg" `shouldBe` "aefg"
    it "should insert the specifed segment" $ do
        splice 1 0 "xyz" "abcdefg" `shouldBe` "axyzbcdefg"
    it "should replace the specifed segment" $ do
        splice 1 3 "xyz" "abcdefg" `shouldBe` "axyzefg"

