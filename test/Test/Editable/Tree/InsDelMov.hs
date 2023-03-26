module Test.Editable.Tree.InsDelMov where
import Test.Hspec (describe, it, shouldBe, SpecWith)
import Data.Tree (unfoldTree, Tree (Node), drawTree)
import Editable.Core ((~), Invertable (invert))
import Editable.Tree.InsDelMov (InsDelMov(..), splitTree, adjustDeletion)
import Data.Functor ((<&>))

printTree :: Show a => Tree a -> IO ()
printTree t = putStrLn $ drawTree $ show <$> t

testSuite :: SpecWith ()
testSuite =
    describe "InsDelMov" $ do
        describe "as Editable" $ do
            let base = unfoldTree (\i -> (i, [i + 1..3])) (0::Int)
            describe "Insert" $ do
                it "should insert the subtree at the given location" $ do
                    Just base ~ Insert [0] (Node (4 :: Int) []) `shouldBe` Just
                        ( Node 0
                            [ Node 4 []
                            , Node 1
                                [ Node 2 [Node 3 []]
                                , Node 3 [] ]
                            , Node 2 [Node 3 []]
                            , Node 3 [] ] )
                    Just base ~ Insert [0, 1] (Node (4 :: Int) [Node 5 []]) `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 [Node 3 []]
                                , Node 4 [Node 5 []]
                                , Node 3 [] ]
                            , Node 2 [Node 3 []]
                            , Node 3 [] ] )
                    Just base ~ Insert [2, 0] (Node (4 :: Int) [Node 5 []]) `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 [Node 3 []]
                                , Node 3 [] ]
                            , Node 2 [Node 3 []]
                            , Node 3 
                                [ Node 4
                                    [ Node 5 [] ] ] ] )
                it "should fail if the location is illegal" $ do
                    Just base ~ Insert [] (Node (4 :: Int) []) `shouldBe` Nothing
                    Just base ~ Insert [-1] (Node (4 :: Int) []) `shouldBe` Nothing
                    Just base ~ Insert [0, 3] (Node (4 :: Int) []) `shouldBe` Nothing
                    Just base ~ Insert [3, 0] (Node (4 :: Int) []) `shouldBe` Nothing

            describe "Delete" $ do
                it "should delete the subtree from the given location" $ do
                    Just base ~ Delete [0] (Node (1 :: Int) [ Node 2 [Node 3 []], Node 3 []]) `shouldBe` Just
                        ( Node 0
                            [ Node 2 [Node 3 []]
                            , Node 3 [] ] )
                    Just base ~ Delete [0, 1] (Node (3 :: Int) []) `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 [Node 3 []] ]
                            , Node 2 [Node 3 []]
                            , Node 3 [] ] )
                it "should fail if the subtree does not match" $ do
                    Just base ~ Delete [0, 1] (Node (4 :: Int) []) `shouldBe` Nothing
                    Just base ~ Delete [0, 1] (Node (3 :: Int) [ Node 4 [] ]) `shouldBe` Nothing
                    Just base ~ Delete [0] (Node (1 :: Int) [ Node 2 [Node 3 []], Node 4 []]) `shouldBe` Nothing
                it "should fail if the location is illegal" $ do
                    Just base ~ Delete [] (Node (4 :: Int) []) `shouldBe` Nothing
                    Just base ~ Delete [-1] (Node (4 :: Int) []) `shouldBe` Nothing
                    Just base ~ Delete [0, 2] (Node (4 :: Int) []) `shouldBe` Nothing
                    Just base ~ Delete [2, 0] (Node (4 :: Int) []) `shouldBe` Nothing

            describe "Move" $ do
                it "should move the subtree to the given location" $ do
                    Just base ~ (Move [2] [0] :: InsDelMov Int) `shouldBe` Just
                        ( Node 0
                            [ Node 3 [] 
                            , Node 1
                                [ Node 2 [Node 3 []]
                                , Node 3 [] ]
                            , Node 2 [Node 3 []] ] )
                    Just base ~ (Move [1] [0, 1] :: InsDelMov Int) `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 [Node 3 []]
                                , Node 2 [Node 3 []]
                                , Node 3 [] ]
                            , Node 3 [] ] )
                    Just base ~ (Move [0, 0, 0] [0, 1] :: InsDelMov Int) `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 []
                                , Node 3 []
                                , Node 3 [] ]
                            , Node 2 [Node 3 []]
                            , Node 3 [] ] )
                it "should fail if the moving forms a cycle" $ do
                    Just base ~ (Move [0, 1] [0, 1, 0] :: InsDelMov Int) `shouldBe` Nothing
                    Just base ~ (Move [0] [0, 0, 2] :: InsDelMov Int) `shouldBe` Nothing
                it "should fail if the location is illegal" $ do
                    Just base ~ (Move [-1] [0] :: InsDelMov Int) `shouldBe` Nothing
                    Just base ~ (Move [0] [-1] :: InsDelMov Int) `shouldBe` Nothing
                    Just base ~ (Move [0] [2, 1] :: InsDelMov Int) `shouldBe` Nothing
                    Just base ~ (Move [2, 0] [0] :: InsDelMov Int) `shouldBe` Nothing

            describe "NoChange" $ do
                it "should keep the tree unchanged" $ do
                    Just base ~ (NoChange :: InsDelMov Int) `shouldBe` Just
                        ( Node 0
                            [ Node 1
                                [ Node 2 [Node 3 []]
                                , Node 3 [] ]
                            , Node 2 [Node 3 []]
                            , Node 3 [] ] )
        describe "As Invertable" $ do
            it "should invert the operations correctly" $ do
                invert (NoChange :: InsDelMov Int) `shouldBe` NoChange
                invert (Insert [0, 1] (Node (3 :: Int) [])) `shouldBe` Delete [0, 1] (Node (3 :: Int) [])
                invert (Delete [0, 1] (Node (3 :: Int) [])) `shouldBe` Insert [0, 1] (Node (3 :: Int) [])
                invert (Move [0, 1] [2, 3] :: InsDelMov Int) `shouldBe` Move [2, 3] [0, 1]