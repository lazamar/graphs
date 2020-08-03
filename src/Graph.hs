{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Graph where

import Control.Monad (foldM, forM, unless)
import Control.Monad.ST
import Data.Array (Array, (!))
import Data.Array.MArray (MArray)
import Data.Array.ST (STArray, runSTArray)
import Data.Bifunctor (second)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Heap (Heap)
import Data.List (minimumBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (listToMaybe)
import Data.STRef
import Data.Set (Set)
import Data.Traversable (for)
import qualified Data.Array as Array
import qualified Data.Array.MArray as MArray
import qualified Data.Heap as Heap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set

type Bounds = (Int , Int)

type Vertex = Int

type Table a = Array Vertex a

type Graph e = Table [(e , Vertex)]

type Edge e = (Vertex , e , Vertex)

type LabelledEdge e = (String , e , String)

type Labelling = (Vertex -> String , String -> Vertex)

data Labelled e = Labelled
    { _labelling :: Labelling
    , _graph :: Graph e
    }

data Tree = Node Vertex Forest deriving (Show)

type Forest = [Tree]

---------------------------------------------------------
-- Utilities

vertices :: Graph e -> [Vertex]
vertices = Array.indices

edges :: Graph e -> [Edge e]
edges g =
    [ (v , e , t)
    | v <- vertices g
    , (e , t) <- g ! v
    ]

-- | O(1)
successors :: Graph a -> Vertex -> [(a , Vertex)]
successors g v = g ! v

reverseE :: Graph e -> [Edge e]
reverseE = map rev . edges
    where
        rev (v , e , t) = (t , e , v)

bounds :: Graph e -> Bounds
bounds = Array.bounds

buildWithLabels :: [LabelledEdge e] -> Labelled e
buildWithLabels labelled = Labelled (toLabel , toVertex) $ buildG' unlabelled
    where
        unlabelled = [(toVertex from , e , toVertex to) | (from , e , to) <- labelled]
        toVertex l = l `Set.findIndex` labels
        toLabel ix = ix `Set.elemAt` labels
        labels = fold [Set.singleton from <> Set.singleton to | (from , e , to) <- labelled]

-- | build a graph from a list of edges
buildG :: Bounds -> [Edge a] -> Graph a
buildG b es = Array.accumArray (flip (:)) [] b [(i , (e , v)) | (i , e , v) <- es]

buildG' :: [Edge a] -> Graph a
buildG' es = buildG (boundsFromEdges es) es

boundsFromEdges :: [Edge a] -> Bounds
boundsFromEdges es = (minimum vs , maximum vs)
    where
        vs = [w | (v , _ , t) <- es , w <- [v , t]]

---------------------------------------------------------
-- Base operations

-- | Create a possibly infinite Tree departing from a graph's node
generate :: Graph e -> Vertex -> Tree
generate g v = Node v $ map (generate g . snd) (g ! v)

-- | Prune a forest in a depth-first way, removing visited nodes
prune :: Forest -> Forest
prune aforest = runST $ do
    visitedRef <- newSTRef (mempty :: Set Vertex)
    let chop forest = case forest of
            [] -> return []
            (Node vertex children : siblings) -> do
                visited <- readSTRef visitedRef
                if vertex `Set.member` visited
                    then chop siblings
                    else do
                        modifySTRef visitedRef (Set.insert vertex)
                        children' <- chop children
                        siblings' <- chop siblings
                        return $ Node vertex children' : siblings'
    chop aforest

foldGAll
    :: forall edge result. Eq result
    => result
    -> (Vertex -> [(edge , result)] -> result)
    -> Graph edge
    -> Table result
foldGAll startVal f graph = runSTArray $ do
    results <- MArray.newArray (bounds graph) startVal
    let loop = do
            old <- for ordered (MArray.readArray results)
            for ordered $ step results
            new <- for ordered (MArray.readArray results)
            if (any changed $ zip old new)
                then loop
                else return results
    loop
    where
        ordered = preorder graph

        changed (before , after) = before /= after

        step results vertex = do
            vbefore <- MArray.readArray results vertex
            succ <- for (successors graph vertex) $ traverse (MArray.readArray results)
            MArray.writeArray results vertex $ f vertex succ

---------------------------------------------------------
--  Algorithms

transpose :: Graph a -> Graph a
transpose = buildG' . reverseE

outdegree :: Graph a -> Table Int
outdegree = fmap length

indegree :: Graph a -> Table Int
indegree = outdegree . transpose

undirected :: Graph a -> Graph a
undirected g = buildG' $ edges g ++ reverseE g

-- | Depth-first search over specific vertices
dfs :: Graph e -> [Vertex] -> Forest
dfs g vs = prune $ map (generate g) vs

dff :: Graph e -> Forest
dff g = dfs g (reverse $ vertices g)

preorder :: Graph a -> [Vertex]
preorder = concatMap pre . dff
    where
        pre (Node v children) = v : concatMap pre children

postorder :: Graph a -> [Vertex]
postorder = concatMap post . dff
    where
        post (Node v children) = concatMap post children ++ [v]

topologicalSort :: Graph e -> [Vertex]
topologicalSort = reverse . postorder

scc :: Graph a -> Forest
scc g = dfs g (postorder g)

components :: Graph a -> Forest
components = dff . undirected

bfs :: Vertex -> Graph a -> [Vertex]
bfs v = map head . bft [v]

bft :: [Vertex] -> Graph a -> [[Vertex]]
bft vs g = go mempty $ map pure vs
    where
        go _ [] = []
        go visited (p : ps) =
            if Set.member v visited
                then go visited ps
                else p : go (Set.insert v visited) (ps ++ map (: p) children)
            where
                v = head p
                children = map snd $ g ! v

-- | Shortest path by length, without taking edge weights into account
-- shortestPath :: Vertex -> Vertex -> Graph a -> Maybe [Vertex]
-- shortestPath from to = listToMaybe . filter ((== to) . head) . bft [from]
newtype Path w = Path {unPath :: NonEmpty (Vertex , w)}

pathWeight :: Path e -> e
pathWeight = snd . NonEmpty.head . unPath

pathTarget :: Path e -> Vertex
pathTarget = fst . NonEmpty.head . unPath

instance Eq e => Eq (Path e) where
    (==) = (==) `on` pathWeight

instance Ord e => Ord (Path e) where
    compare = compare `on` pathWeight

shortestPath :: Real e => Vertex -> Vertex -> Graph e -> Maybe [Vertex]
shortestPath from to =
    fmap (map fst . NonEmpty.toList . unPath)
        . listToMaybe
        . filter ((to ==) . pathTarget)
        . pathsFrom from

-- | Get weighted paths from origin sorted by weight
-- Includes paths from all nodes in the graph
pathsFrom
    :: forall e. Real e
    => Vertex
    -> Graph e
    -> [Path e]
pathsFrom origin graph = dijkstra mempty $ Heap.singleton $ Path ((origin , 0 :: e) :| mempty)
    where
        dijkstra :: Set Vertex -> Heap (Path e) -> [Path e]
        dijkstra visited paths = case Heap.viewMin paths of
            Nothing -> []
            Just (shortest , others) ->
                let visited' = Set.insert (pathTarget shortest) visited
                    newPaths = foldMap Heap.singleton (extendPath visited' shortest)
                 in shortest : dijkstra visited' (others <> newPaths)
        extendPath :: Set Vertex -> Path e -> [Path e]
        extendPath visited p =
            map addEdge $
                filter (not . (`Set.member` visited) . snd) $
                    successors graph (pathTarget p)
            where
                pathCons node = Path . NonEmpty.cons node . unPath
                addEdge (e , v) = pathCons (v , e + pathWeight p) p

-- | Djikstra's shortest path using fixpoint function
shortestPath' :: forall e. Real e => Vertex -> Vertex -> Graph e -> Maybe [Vertex]
shortestPath' from to graph = fmap snd $ foldGAll Nothing f graph ! from
    where
        f :: Vertex -> [(e , Maybe (e , [Vertex]))] -> Maybe (e , [Vertex])
        f vertex successors
            | vertex == to = Just (0 , [vertex])
            | not (null paths) = Just $ minimumBy (compare `on` fst) paths
            | otherwise = Nothing
            where
                paths =
                    [ (cost + edge , vertex : path)
                    | (cost , Just (edge , path)) <- successors
                    ]
