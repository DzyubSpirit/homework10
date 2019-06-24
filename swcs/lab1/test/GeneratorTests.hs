module GeneratorTests where

import Protolude
import Test.Hspec
import System.Random
import Data.List (nub)
import Data.Graph.Inductive.PatriciaTree
import Data.Maybe (fromJust)

import qualified Data.Graph.Inductive.Graph as G

import IGE.Types
import Generator

votes g = do
  describe "shuffleList" $ do
    it "empty list" $ fst (shuffleList g []) `shouldBe` ([] :: [Int])
    it "one-element list" $ fst (shuffleList g [1]) `shouldBe` [1]
    it "two-element list"
      $          length (nub $ fst $ shuffleList g [1, 2])
      `shouldBe` 2
    it "four-element list"
      $          length (nub $ fst $ shuffleList g [1 .. 4])
      `shouldBe` 4
  describe "downvote" $ do
    it "leaves exact sum unchanges"
      $          downvote (Weight 0, Weight 15) (Weight 3) [(0, 0, Weight 3)]
      `shouldBe` [(0, 0, Weight 3)]
    it "downvotes one edge"
      $          downvote (Weight 0, Weight 15) (Weight 1) [(0, 0, Weight 3)]
      `shouldBe` [(0, 0, Weight 1)]
    it "downvotes two different edges"
      $          downvote (Weight 1, Weight 15)
                          (Weight 2)
                          [(0, 0, Weight 3), (1, 1, Weight 2)]
      `shouldBe` [(0, 0, Weight 1), (1, 1, Weight 1)]
  describe "upvote" $ do
    it "leaves exact sum unchanges"
      $          downvote (Weight 0, Weight 15) (Weight 3) [(0, 0, Weight 3)]
      `shouldBe` [(0, 0, Weight 3)]
    it "upvotes one edge"
      $          downvote (Weight 0, Weight 15) (Weight 5) [(0, 0, Weight 3)]
      `shouldBe` [(0, 0, Weight 5)]
    it "upvotes two different edges"
      $          downvote (Weight 1, Weight 7)
                          (Weight 10)
                          [(0, 0, Weight 6), (1, 1, Weight 2)]
      `shouldBe` [(0, 0, Weight 7), (1, 1, Weight 3)]
    it "upvotes two edges"
      $          sum
                   ( map G.edgeLabel $ upvote
                     (Weight 0, Weight 6)
                     (Weight 6)
                     [(1, 2, Weight 1), (2, 3, Weight 1), (1, 3, Weight 3)]
                   )
      `shouldBe` Weight 6
  describe "tune"
    $          it "simple test"
    $          fmap
                 (sum . map G.edgeLabel)
                 ( fst $ tune (Weight 6)
                              (Weight 0, Weight 6)
                              [(1, 2, Weight 1), (2, 3, Weight 1)]
                              g
                 )
    `shouldBe` Just (Weight 6)
  describe "randomTaskGraph" $ do
    let gp2 = GenParams 2 (1, 5) 0.5 Nothing
        gr2 = fst $ randomTaskGraph g gp2 :: Maybe (Gr Weight Weight)
    it "length of two node random graph should be 2"
      $          fmap G.noNodes gr2
      `shouldBe` Just 2
    it "node weight should be in range for two node random graph"
      $ fmap (all (\(_, x) -> x >= Weight 1 && x <= Weight 5) . G.labNodes) gr2
      `shouldBe` Just True
    it "sum of the node weights and sum of the edge weights should be equal"
      $          fmap
                   ( \gr -> sum (map snd $ G.labNodes gr)
                     == sum (G.edgeLabel <$> G.labEdges gr)
                   )
                   gr2
      `shouldBe` Just True
    let gr3 =
          fst $ randomTaskGraph g (GenParams 4 (5, 15) 0.5 $ Just (5, 15)) :: Maybe
              (Gr Weight Weight)
    it "graph from default params should have 4 nodes"
      $          fmap G.noNodes gr3
      `shouldBe` Just 4
  describe "randomSysGraph" $ do
    let gr3 =
          fst $ randomSysGraph g (GenParams 4 (5, 15) 0.5 $ Just (5, 15)) :: Maybe
              (Gr Weight Weight)
    it "graph from default params should have 4 nodes"
      $          fmap G.noNodes gr3
      `shouldBe` Just 4
    it
        "graph from default params should have edges with sum of node weights and sum of edge weights"
      $          ((* 2) . sum . map snd . G.labNodes <$> gr3)
      `shouldBe` (sum . map G.edgeLabel . G.labEdges <$> gr3)
