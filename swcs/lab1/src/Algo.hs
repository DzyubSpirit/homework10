module Algo where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import IGE.Types

import Protolude
import Data.List (groupBy)
import Data.Maybe (fromJust)
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
  where em = edgesMap $ labEdges gr

edgesMap :: [(Node, Node, Weight)] -> M.Map Node [Node]
edgesMap = foldr (\(f, t, _) -> M.alter (Just . maybe [t] (t :)) f) M.empty

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
  em = edgesMap $ labEdges gr
  nc = length $ labNodes gr

taskQueue :: Graph gr => gr Weight Weight -> [Node]
taskQueue gr = map fst $ sortBy sDfASort $ M.toList $ M.map
  (\(t, c) -> fromIntegral t / gt + fromIntegral c / gt)
  cps
 where
  cps = criticalPaths gr
  gt  = fromIntegral $ M.foldr (+) 0 $ M.map fst cps
  gc  = fromIntegral $ M.foldr (+) 0 $ M.map snd cps
  -- second descendant, first ascending
  sDfASort (fa, sa) (fb, sb) =
    let se = compare sb sa in if se /= EQ then se else compare fa fb

criticalPaths :: Graph gr => gr Weight Weight -> M.Map Node (Weight, Weight)
criticalPaths gr = M.foldrWithKey (\k v -> M.insert k (v, cs M.! k)) M.empty ts
 where
  nm  = M.fromList $ labNodes gr
  iem = edgesMap $ (\(a, b, c) -> (b, a, c)) <$> labEdges gr
  em  = edgesMap $ labEdges gr
  ts =
    snd $ fromJust $ head $ dropWhile ((< M.size nm) . M.size . snd) $ iterate
      (nextNodes (nm M.!))
      (M.empty, M.empty)
  cs =
    snd $ fromJust $ head $ dropWhile ((< M.size nm) . M.size . snd) $ iterate
      (nextNodes $ const 1)
      (M.empty, M.empty)
  nextNodes upd (times, paths) = foldr
    ( \(n, t', nns) (ts, ps) ->
      (foldr (M.alter (Just . maybe t' (max t'))) ts nns, M.insert n t' ps)
    )
    (times, paths)
    changes
   where
    changes =
      map
          ( \n ->
            let t' = upd n + fromMaybe 0 (M.lookup n times)
            in  (n, t', fromMaybe [] $ M.lookup n iem)
          )
        $ filter
            ( \n ->
              let isntUsed = not (M.member n paths)
                  isLeaf   = maybe True (all (`M.member` paths)) $ M.lookup n em
              in  isntUsed && isLeaf
            )
        $ map fst
        $ labNodes gr
