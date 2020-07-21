module GraphDFS where

import qualified Data.Array as Array
import Control.Monad (guard)
import Control.Monad.ST
import Data.Array (Array, array, (!))
import Data.Bifunctor (second, bimap)
import Data.List (sortBy, minimumBy, nub)
import Data.Map (Map)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.STRef
import Data.Traversable (for)
import Data.Tuple (swap)
import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Array as Array
import qualified Data.Set as Set

type Bounds             = (Int, Int)
type Vertex             = Int
type Table a            = Array Vertex a
type Graph e            = Table [(e, Vertex)]
type Edge e             = (Vertex, e, Vertex)
type Labelling l        = Vertex -> l
data Labelled e l  = Labelled
    { _labelling :: Labelling l
    , _graph :: Graph e
    }

data Tree     = Node Vertex Forest
type Forest   = [Tree]


---------------------------------------------------------
-- Utilities

vertices :: Graph e -> [Vertex]
vertices = Array.indices

edges :: Graph e -> [Edge e]
edges g =
    [ (v, e, t)
    | v <- vertices g
    , (e, t) <- g ! v
    ]

bounds :: Graph e -> Bounds
bounds = Array.bounds

-- | build a graph from a list of edges
buildG :: Bounds -> [Edge a] -> Graph a
buildG b edges = Array.accumArray (flip (:)) [] b [(i, (e, v)) | (i, e, v) <- edges ]

buildG' :: [Edge a] -> Graph a
buildG' edges = buildG b edges
    where
        vs = concatMap (\(v,_,t) -> [v,t]) edges
        b  = (minimum vs, maximum vs)

---------------------------------------------------------
-- Base operations

-- | Create a possibly infinite Tree departing from a graph's node
generate :: Graph e -> Vertex -> Tree
generate g v = Node v $ map (generate g . snd) (g ! v)

-- | Prune a forest in a depth-first way, removing visited nodes
prune :: Forest -> Forest
prune forest = runST $ do
    visitedRef <- newSTRef (mempty :: Set Vertex)
    let
        chop forest = case forest of
            [] -> return []
            (Node vertex children : siblings) -> do
                visited <- readSTRef visitedRef
                if vertex `Set.member` visited then
                    return []
                else do
                    modifySTRef visitedRef (Set.insert vertex)
                    children' <- chop children
                    siblings' <- chop siblings
                    return $ Node vertex children' : siblings'
    chop forest


-- | Depth-first search over specific vertices
dfs :: Graph e -> [Vertex] -> Forest
dfs g vs = prune $ map (generate g) vs

dff :: Graph e -> Forest
dff g = dfs g (vertices g)

---------------------------------------------------------
-- Displaying
visualiseGraph undirected (Labelled lab rawG)  =
    unlines
        [ "digraph name {"
        , "rankdir=LR;"
        , unlines $ map showNode $ vertices graph
        , unlines $ map showEdge $ edges graph
        , "}"
        ]
    where
        graph = if undirected
            then buildG' $ nub $ map ordered $ edges rawG
            else rawG

        ordered (v, e, t) = if v > t then (v, e, t) else (t, e, v)

        showEdge (from, t, to) = unwords
            [ show from , "->" , show to , "["
            , "label = \"" ++ show t ++ "\""
            , if undirected then ", arrowhead=none" else ""
            , "];"
            ]
        showNode v = unwords [ show v , "[ label = " ++ (show $ lab v) , "];" ]

visualiseForest :: Show l => Labelling l -> Forest -> String
visualiseForest l = visualiseGraph True . Labelled l . fromForest

fromForest :: Forest -> Graph ()
fromForest forest = buildG' $ toEdges forest []
    where
        toEdges [] = id
        toEdges (Node v children:siblings) =
            (++ [(v, (), c) | Node c _ <- children])
            . toEdges children
            . toEdges siblings

mkLabel :: Ord a => [(a, e, a)] -> (Labelling a, [Edge e])
mkLabel raw = (toLabel, map withLabels raw)
    where
        withLabels (v,e,t) = (fromLabel v, e, fromLabel t)

        numberNodePairs
            = zip [0..]
            $ nub
            $ concatMap (\(v,_,t) -> [v,t]) raw

        vertexToNodeMap = Map.fromList numberNodePairs
        nodeToVertexMap = Map.fromList $ map swap numberNodePairs

        fromLabel = ((Map.!) nodeToVertexMap)
        toLabel   = ((Map.!) vertexToNodeMap)

fromAssocList :: Ord a => [(a,[a])] -> Labelled () a
fromAssocList ass = Labelled labelling graph
    where
        graph = buildG' edges
        (labelling, edges) = mkLabel tt
        tt = [(v,(),t) | (v, cs) <- ass, t <- cs]

main :: IO ()
main = putStrLn $ visualiseGraph False spjGraph
    where
        spjGraph = fromAssocList
            [ ("a", ["j", "g"])
            , ("b", ["a", "i"])
            , ("c", ["h", "e"])
            , ("e", ["d","h","j"])
            , ("f", ["i"])
            , ("g", ["f", "b"])
            ]

        weightedGraph = buildG (1,5)
            [ (1,1,2)
            , (2,2,3)
            , (2,2,5)
            , (3,3,4)
            , (4,4,5)
            ]

        graph =
            buildG (1,8) . nub . map ordered $
            [ (1, 0, 2)
            , (1, 0, 3)
            , (1, 0, 4)
            , (2, 0, 1)
            , (2, 0, 6)
            , (3, 0, 1)
            , (3, 0, 4)
            , (3, 0, 5)
            , (3, 0, 6)
            , (4, 0, 1)
            , (4, 0, 3)
            , (4, 0, 5)
            , (5, 0, 3)
            , (5, 0, 4)
            , (5, 0, 6)
            , (5, 0, 7)
            , (5, 0, 8)
            , (6, 0, 2)
            , (6, 0, 3)
            , (6, 0, 5)
            , (6, 0, 7)
            , (7, 0, 5)
            , (7, 0, 6)
            , (7, 2, 8)
            , (8, 0, 4)
            , (8, 0, 5)
            , (8, 2, 7)
            ]
        ordered (v, e, t) = if v > t then (v, e, t) else (t, e, v)
