module Main where

import Test.Hspec ( hspec, describe, it, shouldBe )
import qualified Test.Editable.List.Replace
import qualified Test.Editable.List.Util
import qualified Test.Editable.List.InsDel
import qualified Test.Editable.Scalar.Replace
import qualified Test.Editable.Map.Replace
import qualified Test.Editable.Tree.InsDelMov


main :: IO ()
main = hspec $ do
    Test.Editable.List.Util.testSuite
    Test.Editable.List.Replace.testSuite
    Test.Editable.List.InsDel.testSuite
    Test.Editable.Scalar.Replace.testSuite
    Test.Editable.Map.Replace.testSuite
    Test.Editable.Tree.InsDelMov.testSuite
