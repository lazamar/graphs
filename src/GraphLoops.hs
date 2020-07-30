{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module GraphLoops where

import qualified Data.Array as Array
import Control.Monad (guard)
import Control.Monad.ST
import Data.Array (Array, array, bounds, (!))
import Data.Bifunctor (second, bimap)
import Data.List (sortBy, minimumBy, nub)
import Data.Map (Map)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.STRef
import Data.Traversable (for)
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
data LabelledGraph e l  = LabelledGraph
    { _graph :: Graph e
    , _labelling :: Labelling l
    }

foldG :: Eq result => result -> (Vertex -> [(edge, result)] -> result) -> Graph edge -> Vertex -> result
foldG startVal f graph root = foldGAll startVal f graph ! root

foldGAll :: forall edge result. Eq result => result -> (Vertex -> [(edge, result)] -> result) -> Graph edge -> Table result
foldGAll startVal f graph = finalTable
    where
        finalTable   = fixedPoint updateTable initialTable
        initialTable = fmap (const startVal) graph

        updateTable table = results
            where
                lastResultsFor i = map (second (table !)) $ graph ! i
                results
                    = Array.listArray (bounds graph)
                    $ map (\i -> f i $ lastResultsFor i)
                    $ Array.indices table

        fixedPoint fun last =
            let this = fun last
            in
            if this == last then this else fixedPoint fun this

unfoldG :: Ord seed => (seed -> (label, [(edge, seed)])) -> seed -> (Vertex, LabelledGraph edge label)
unfoldG gen seed = (root, graph)
    where
        ([root], graph) = unfoldGAll gen [seed]

unfoldGAll :: forall seed label edge. Ord seed => (seed -> (label, [(edge, seed)])) -> [seed] -> ([Vertex], LabelledGraph edge label)
unfoldGAll gen initialSeeds = runST $ do
    let firstId = 0 :: Vertex
    adjacencyRef   <- newSTRef (mempty :: [(Vertex, label, [(edge, Vertex)])])
    linksRef       <- newSTRef (mempty :: Map seed Vertex)
    currentNodeRef <- newSTRef firstId
    let allocate = do
            vertex <- readSTRef currentNodeRef
            writeSTRef currentNodeRef (vertex + 1)
            return vertex

        build seed = do
            links <- readSTRef linksRef
            case Map.lookup seed links of
                Just vertex -> return vertex
                Nothing -> do
                    vertex <- allocate
                    modifySTRef linksRef $ Map.insert seed vertex
                    let (label, dependencies) = gen seed
                        (edges, seeds) = unzip dependencies
                    children <- traverse build seeds
                    modifySTRef adjacencyRef $ ((vertex, label, zip edges children):)
                    return vertex
    initialSeedsResults <- traverse build initialSeeds
    adjacencyList <- readSTRef adjacencyRef
    lastId <- pred <$> readSTRef currentNodeRef
    let graph  = array (firstId, lastId) [(vertex, links) | (vertex, _, links) <- adjacencyList]
        labels = array (firstId, lastId) [(vertex, label) | (vertex, label, _) <- adjacencyList]
    return $ (initialSeedsResults, LabelledGraph graph (labels !))

-- | build a graph from a list of edges
buildG :: Bounds -> [Edge a] -> Graph a
buildG b edges = Array.accumArray (flip (:)) [] b [(i, (e, v)) | (i, e, v) <- edges ]

edges :: Graph a -> [Edge a]
edges graph = [ (i, e, v) |  (i, es) <- Array.assocs graph , (e, v) <- es ]

vertices :: Graph a -> [Vertex]
vertices = Array.indices

reverseE :: Edge a -> Edge a
reverseE (i, e, v) = (v, e, i)

onGraph :: (Graph a -> Graph a) -> LabelledGraph a b -> LabelledGraph a b
onGraph f g = g { _graph = f $ _graph g }

-----------------------------------------------------------------------------------------------------
-- Operations on graphs

reverseEdges :: Graph a -> Graph a
reverseEdges graph = buildG (Array.bounds graph) $ map reverseE $ edges graph

closure :: Vertex -> Graph a -> Graph a
closure root graph = _graph $ snd $ unfoldG (\v -> (v, graph ! v)) root

shortestPath :: Graph Int -> Vertex -> Vertex -> Maybe (Int, [Vertex])
shortestPath graph from to = foldG Nothing f graph from
    where
        f vertex children =
            if vertex == to
               then Just (0, [to])
               else guard (not $ null paths) >> Just (minimumBy (comparing fst) paths)
            where
                -- weight of each child's path to the target
                paths =
                    [ (weight + pweight, vertex:pnodes)
                    | (weight, Just (pweight, pnodes)) <- children
                    ]

showGraphViz (LabelledGraph gr lab)  =
    "digraph name {\n" ++
    "rankdir=LR;\n" ++ (concatMap showNode $ Array.indices gr) ++ (concatMap showEdge $ edges gr) ++ "}\n"
    where
        showEdge (from, t, to) = show from ++ " -> " ++ show to ++ " [label = \"" ++ show t ++ "\"];\n"
        showNode v = show v ++ " [label = " ++ (show $ lab v) ++ "];\n"

main = putStr $ showGraphViz $ (`LabelledGraph` id) $ bigG
    where
        direct (from,w,to) =
            if from < to
               then (from,w,to)
               else (to,w,from)

        bigG
          = buildG (1,8)
          $ [ (1, 0, 2)
            , (1, 0, 3)
            , (1, -1, 4)
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
            , (6, -1, 3)
            , (6, 0, 5)
            , (6, 0, 7)
            , (7, 0, 5)
            , (7, 0, 6)
            , (7, 0, 8)
            , (8, 0, 4)
            , (8, 0, 5)
            , (8, 2, 7)
            ]

        weightedGraph = buildG (1,5)
            [ (1,1,2)
            , (2,2,3)
            , (2,2,5)
            , (3,3,4)
            , (4,4,5)
            ]

