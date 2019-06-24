module Generator where

import System.Random
import Data.List ((!!), nub)
import Data.Maybe (fromJust)

import qualified Data.Map.Strict as M
import qualified Data.Graph.Inductive.Graph as G

import IGE.Types

import Protolude

data GenParams = GenParams
  { _nodesNum :: Int
  , _nodesWeightRange :: (Weight, Weight)
  , _graphConnectivity :: Double
  , _edgesWeightRange :: Maybe (Weight, Weight)
  }

shuffleList :: RandomGen g => g -> [a] -> ([a], g)
shuffleList g []  = ([], g)
shuffleList g [x] = ([x], g)
shuffleList g xs  = snd $ fromJust $ head $ drop (length xs) $ iterate
  ( \(im, (xs, g)) ->
    let (ix, g') = randomR (0, M.size im - 1) g
        k        = (M.keys im !! ix)
    in  (M.delete k im, (im M.! k : xs, g'))
  )
  (im, ([], g))
  where im = M.fromList $ zip [0 .. length xs - 1] xs

randomList :: (RandomGen g, Random a) => (a, a) -> Int -> g -> ([a], g)
randomList r n g = (map fst xs, snd $ fromJust $ head $ drop (n - 1) xs)
  where xs = take n $ iterate (\(_, g) -> randomR r g) $ randomR r g

instance Random Weight where
  random = first Weight . random
  randomR r = first Weight . randomR (bimap weight weight r)


third :: (c -> d) -> (a, b, c) -> (a, b, d)
third f (x, y, z) = (x, y, f z)

downvote :: (Weight, Weight) -> Weight -> [G.LEdge Weight] -> [G.LEdge Weight]
downvote (l, _) s initEs =
  snd $ fromJust $ head $ dropWhile fst $ iterate (downvote' . snd) $ downvote'
    initEs
 where
  downvote' es = if nb * (ll - l) < diff
    then (True, third (\x -> if x == l then x else x - (ll - l)) <$> es)
    else
      let fes = third (\x -> if x == l then x else x - (diff `div` nb)) <$> es
      in  ( False
          , snd $ foldr
            ( \(f, t, x) (rp, xs) -> if rp == 0 || x == l
              then (rp, (f, t, x) : xs)
              else (rp - 1, (f, t, x - 1) : xs)
            )
            (diff `mod` nb, [])
            fes
          )
   where
    els  = fmap G.edgeLabel es
    diff = sum els - s
    ll   = minimum $ filter (> l) els
    nb   = Weight $ length $ filter (>= ll) els

upvote :: (Weight, Weight) -> Weight -> [G.LEdge Weight] -> [G.LEdge Weight]
upvote (_, h) s initEs =
  snd $ fromJust $ head $ dropWhile fst $ iterate (upvote' . snd) $ upvote'
    initEs
 where
  upvote' es = if nb * (h - hh) <= diff
    then (True, third (\x -> if x == h then x else x + h - hh) <$> es)
    else
      let fes = third (\x -> if x == h then x else x + (diff `div` nb)) <$> es
      in  ( False
          , snd $ foldr
            ( \(f, t, x) (rp, xs) -> if rp == 0 || x == h
              then (rp, (f, t, x) : xs)
              else (rp - 1, (f, t, x + 1) : xs)
            )
            (diff `mod` nb, [])
            fes
          )
   where
    els  = fmap G.edgeLabel es
    diff = s - sum els
    hh   = maximum $ filter (< h) els
    nb   = Weight $ length $ filter (<= hh) els

isAcyclic :: G.Edge -> [G.Edge] -> Bool
isAcyclic (f, t) es = all (notElem f) $ takeWhile (not . null) $ iterate
  nextNodes
  [t]
 where
  nextNodes :: [G.Node] -> [G.Node]
  nextNodes ns = [ t | (f, t) <- es, f `elem` ns ]

newAcyclicEdge
  :: RandomGen g
  => (Weight, Weight)
  -> [G.Edge]
  -> g
  -> Maybe (G.LEdge Weight, g)
newAcyclicEdge r es g | null allowedEdges = Nothing
                      | otherwise = Just (G.toLEdge (allowedEdges !! ix) w, g3)
 where
  ns = nub $ map fst es ++ map snd es
  allowedEdges =
    [ (f, t)
    | f <- ns
    , t <- ns
    , f /= t
    , (f, t) `notElem` es
    , isAcyclic (f, t) es
    ]
  (ix, g2) = randomR (0, length allowedEdges - 1) g
  (w , g3) = randomR r g2

tune
  :: RandomGen g
  => Weight
  -> (Weight, Weight)
  -> [G.LEdge Weight]
  -> g
  -> (Maybe [G.LEdge Weight], g)
tune ewSum ewr es g
  | lewSum == ewSum = (Just es, g)
  | lewSum > ewSum = (Just $ downvote ewr ewSum es, g)
  | lewSum < ewSum = case newAcyclicEdge ewr (map G.toEdge es) g of
    Just (e, g') -> tune ewSum ewr (e : es) g'
    Nothing      -> if Weight (length es) * snd ewr < ewSum
      then (Nothing, g)
      else (Just $ upvote ewr ewSum es, g)
  where lewSum = sum $ fmap G.edgeLabel es

symTune
  :: RandomGen g
  => M.Map G.Edge Bool
  -> Weight
  -> Weight
  -> (Weight, Weight)
  -> [G.LEdge Weight]
  -> g
  -> (Maybe [G.LEdge Weight], g)
symTune pe lewSum ewSum ewr es g
  | lewSum == ewSum = (Just es, g)
  | lewSum > ewSum = (Just $ downvote ewr ewSum es, g)
  | null pe = if Weight (length es) * snd ewr < ewSum
    then (Nothing, g)
    else (Just $ upvote ewr ewSum es, g)
  | lewSum < ewSum = symTune pe' (lewSum + new) ewSum ewr (nle : es) g3
 where
  (ix , g2) = randomR (0, M.size pe - 1) g
  (new, g3) = randomR ewr g2
  ne        = M.keys pe !! ix
  nle       = G.toLEdge (M.keys pe !! ix) new
  pe'       = M.delete ne pe

initEdges
  :: RandomGen g
  => g
  -> GenParams
  -> Weight
  -> (Maybe ([G.LEdge Weight], (Weight, Weight)), g)
initEdges g (GenParams nn nwr gc ewrM) ewSum
  | Weight nn * fst ewr > ewSum = (Nothing, g3)
  | otherwise                   = (Just (es, ewr), g4)
 where
  (nodesOrder, g2) = shuffleList g [1 .. nn]
  addNode (g, nm, em) node = (g3, M.insert node2 True nm, em')
   where
    (ix2, g2) = randomR (0, M.size nm - 1) g
    node2     = M.keys nm !! ix2
    (dir, g3) = random g2
    em'       = if dir then (node, node2) : em else (node2, node) : em
  fn             = fromJust $ head nodesOrder
  (g3, _, em) = foldl addNode (g2, M.singleton fn True, []) $ drop 1 nodesOrder
  meanEdgeWeight = ewSum `div` Weight (length em)
  ewr            = fromMaybe (Weight 0, Weight 2 * meanEdgeWeight) ewrM
  (ews, g4)      = randomList ewr (length em) g3
  es             = zipWith G.toLEdge em ews

randomTaskGraph
  :: (RandomGen g, G.Graph gr)
  => g
  -> GenParams
  -> (Maybe (gr Weight Weight), g)
randomTaskGraph g gp@(GenParams nn nwr gc ewrM) = case esM of
  Nothing        -> (Nothing, g3)
  Just (es, ewr) -> first (fmap withEdges) $ tune ewSum ewr es g3
 where
  (nws, g2) = randomList nwr nn g
  ewSum     = Weight $ round $ fromIntegral (sum nws) / gc * (1 - gc)
  (esM, g3) = initEdges g2 gp ewSum
  withEdges :: G.Graph gr => [G.LEdge Weight] -> gr Weight Weight
  withEdges = G.mkGraph (zip [1 .. nn] nws)

randomSysGraph
  :: (RandomGen g, G.Graph gr)
  => g
  -> GenParams
  -> (Maybe (gr Weight Weight), g)
randomSysGraph g gp@(GenParams nn nwr gc ewrM) = case edgesM of
  Just edges ->
    (Just $ G.mkGraph (zip [1 .. nn] nws) $ makeSymmetric edges, g4)
  Nothing -> (Nothing, g4)
 where
  (nws, g2)         = randomList nwr nn g
  ewSum             = Weight $ round $ fromIntegral (sum nws) / gc * (1 - gc)
  (initEdgesDM, g3) = initEdges g2 gp ewSum

  Just (ide, ewr)   = initEdgesDM
  ie = map (\(f, t, v) -> if f > t then (t, f, v) else (f, t, v)) ide
  unused            = M.difference
    (M.fromList [ ((i, j), True) | i <- [1 .. nn - 1], j <- [i + 1 .. nn] ])
    (M.fromList [ (G.toEdge e, True) | e <- ie ])
  (edgesM, g4) = symTune unused (sum $ map G.edgeLabel ie) ewSum ewr ie g3
  makeSymmetric es = es ++ map (\(f, t, v) -> (t, f, v)) es
