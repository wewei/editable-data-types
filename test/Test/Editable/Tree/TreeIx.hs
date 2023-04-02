module Test.Editable.Tree.TreeIx where
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Editable.Tree.TreeIx (TreeIx(TreeIx), root, (<~), (~>), diff)
import Control.Monad (forM_)

testSuite :: SpecWith ()
testSuite = describe "TreeIx" $ do
    describe "as Monoid" $ do
        it "should confirm to the Monoid law" $ do
            let ix = TreeIx [0, 1]
                iy = TreeIx [2, 3, 4]
                iz = TreeIx [5]
            ix <> (iy <> iz) `shouldBe` (ix <> iy) <> iz
            ix <> root `shouldBe` ix
            root <> ix `shouldBe` ix
    describe "(~>) & (<~)" $ do
        it "should calculate if one node is decendent of the other " $ do
            forM_
                [ ([], [1], True)
                , ([1], [1, 2, 3], True)
                , ([1], [], False)
                , ([1, 2], [1, 3, 4], False)
                ] $ \(xs, ys, result) -> do
                    TreeIx xs <~ TreeIx ys `shouldBe` result
                    TreeIx ys ~> TreeIx xs `shouldBe` result
    describe "diff" $ do
        it "should calculate the common base and the relative paths" $ do
            diff root (TreeIx [1, 2, 3]) `shouldBe` (root, root, TreeIx [1, 2, 3])
            diff (TreeIx [1, 2, 3]) (TreeIx [1, 3, 2]) `shouldBe` (TreeIx [1], TreeIx [2, 3], TreeIx [3, 2])