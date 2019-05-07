import Protolude
import Test.Hspec
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import IGE.Types

import Algo

mkGraph' :: [LNode Weight] -> [LEdge Weight] -> Gr Weight Weight
mkGraph' = mkGraph

main :: IO ()
main = hspec $ do
  describe "isAcyclic" $ do
    it "returns yes for empty graph"
      $          isAcyclic (mkGraph' [] [])
      `shouldBe` True
    it "returns yes for one-node graph"
      $          isAcyclic (mkGraph' [(1, 0)] [])
      `shouldBe` True
    it "returns no for three-node cycle"
      $          isAcyclic
                   (mkGraph' [(1, 0), (2, 0), (3, 0)] [(1, 2, 0), (2, 3, 0), (3, 1, 0)])
      `shouldBe` False
    it "returns no for three-node tree"
      $          isAcyclic
                   (mkGraph' [(1, 0), (2, 0), (3, 0)] [(1, 2, 0), (2, 3, 0), (1, 3, 0)])
      `shouldBe` True
    it "returns no for three-node-cycle-with-separate-node graph"
      $          isAcyclic
                   ( mkGraph' [(1, 0), (2, 0), (3, 0), (4, 0)]
                              [(1, 2, 0), (2, 3, 0), (3, 1, 0)]
                   )
      `shouldBe` False
    it "returns no for three-node-tree-and-separate-node"
      $          isAcyclic
                   ( mkGraph' [(1, 0), (2, 0), (3, 0), (4, 0)]
                              [(1, 2, 0), (2, 3, 0), (1, 3, 0)]
                   )
      `shouldBe` True
  describe "isConnected" $ do
    it "returns yes for empty graph"
      $          isConnected (mkGraph' [] [])
      `shouldBe` True
    it "returns yes for one-node graph"
      $          isConnected (mkGraph' [(1, 0)] [])
      `shouldBe` True
    it "returns no for two-node acyclic graph"
      $          isConnected (mkGraph' [(1, 0), (2, 0)] [(1, 2, 0)])
      `shouldBe` False
    it "returns yes for two-node cyclic graph"
      $          isConnected (mkGraph' [(1, 0), (2, 0)] [(1, 2, 0), (2, 1, 0)])
      `shouldBe` True
    it "returns no for two-node not-connected graph"
      $          isConnected (mkGraph' [(1, 0), (2, 0)] [])
      `shouldBe` False
    it "returns yes for three-node cyclic graph"
      $          isConnected
                   (mkGraph' [(1, 0), (2, 0), (3, 0)] [(1, 2, 0), (2, 3, 0), (3, 1, 0)])
      `shouldBe` True
    it "returns no for three-node not-cyclic graph"
      $          isConnected
                   (mkGraph' [(1, 0), (2, 0), (3, 0)] [(1, 2, 0), (2, 3, 0), (1, 3, 0)])
      `shouldBe` False
    it "returns no for four-node not-connected graph"
      $          isConnected
                   ( mkGraph' [(1, 0), (2, 0), (3, 0), (4, 0)]
                              [(1, 2, 0), (2, 3, 0), (3, 1, 0)]
                   )
      `shouldBe` False
