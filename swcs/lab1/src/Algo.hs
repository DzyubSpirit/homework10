module Algo where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import IGE.Types

import Protolude
import Data.List (groupBy)
import qualified Data.Map.Strict as M
data GraphSettings = GraphSettings
  { nodesNum :: Int
  , nodeWeightRange :: (Weight, Weight)
  , edgesNum :: Int
  , edgeWeightRange :: (Weight,Weight)
  }

--randomGraph :: GraphSettings -> IO (Graph Weight Weight)
--randomGraph = undefined

isAcyclic :: Graph gr => gr Weight Weight -> Bool
isAcyclic gr = all (uncurry isEdgeAcyclic) $ zip edges $ scanr insEdge
                                                               woEdges
                                                               edges
 where
  woEdges = mkGraph (labNodes gr) [] :: Gr Weight Weight
  edges   = labEdges gr

isEdgeAcyclic :: Graph gr => LEdge Weight -> gr Weight Weight -> Bool
isEdgeAcyclic e@(f, t, _) gr =
  not
    $ any (elem f)
    $ takeWhile (not . null)
    $ iterate (nextNodes em)
    $ nextNodes em [f]
  where em = edgesMap gr

edgesMap :: Graph gr => gr Weight Weight -> M.Map Node [Node]
edgesMap =
  foldr (\(f, t, _) -> M.alter (Just . maybe [t] (t :)) f) M.empty . labEdges

nextNodes :: M.Map Node [Node] -> [Node] -> [Node]
nextNodes edges = ordNub . concat . mapMaybe (`M.lookup` edges)

isConnected :: Graph gr => gr Weight Weight -> Bool
isConnected gr =
  and
    $   any ((== nc) . length)
    .   take nc
    .   iterate (\ns -> ordNub $ nextNodes em ns ++ ns)
    .   pure
    .   fst
    <$> labNodes gr
 where
  em = edgesMap gr
  nc = length $ labNodes gr
