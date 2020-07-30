module Main where

import Graph
import qualified Data.Array as Array
import Control.Monad (guard)
import Control.Monad.ST
import Data.Array (Array, array, (!))
import Data.Bifunctor (second, bimap)
import Data.Heap (Heap)
import Data.List (sortBy, minimumBy, nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (mapMaybe, listToMaybe, fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.STRef
import Data.Traversable (for)
import Data.Tuple (swap)
import Debug.Trace

import qualified Data.Array as Array
import qualified Data.Heap as Heap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = -- putStrLn $ visualiseForest kingLabel $ components kingGraph
    print
        $ fmap (map kingLabel)
        $ shortestPath 5 6 kingGraph
    where
        Labelled kingLabel kingGraph = fromAssocList pure
            [ ('a', ['j', 'g'])
            , ('b', ['a', 'i'])
            , ('c', ['h', 'e'])
            , ('e', ['d','h','j'])
            , ('f', ['i'])
            , ('g', ['f', 'b'])
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

        -- Displaying
        visualiseGraph mVertices undirected (Labelled lab rawG)  =
            unlines
                [ "digraph name {"
                , "rankdir=LR;"
                , unlines $ map showNode $ fromMaybe (vertices graph) mVertices
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

        visualiseForest :: Labelling -> Forest -> String
        visualiseForest l forest
            = visualiseGraph (Just $ fvertices forest) False
            $ Labelled l
            $ buildG'
            $ toEdges forest

        fvertices :: Forest -> [Vertex]
        fvertices [] = []
        fvertices (Node v children : siblings) = v : fvertices siblings

        toEdges :: Forest -> [Edge ()]
        toEdges = flip go []
            where
                go [] = id
                go (Node v children:siblings) =
                    (++ [(v, (), c) | Node c _ <- children])
                    . go children
                    . go siblings

        mkLabel :: Ord a => (a -> String) -> [(a, e, a)] -> (Labelling, [Edge e])
        mkLabel showLabel raw = (toLabel, map withLabels raw)
            where
                withLabels (v,e,t) = (fromLabel v, e, fromLabel t)

                numberNodePairs
                    = zip [0..]
                    $ nub
                    $ concatMap (\(v,_,t) -> [v,t]) raw

                vertexToNodeMap = Map.fromList numberNodePairs
                nodeToVertexMap = Map.fromList $ map swap numberNodePairs

                fromLabel = ((Map.!) nodeToVertexMap)
                toLabel   = showLabel . ((Map.!) vertexToNodeMap)

        fromAssocList :: Ord a => (a -> String) -> [(a,[a])] -> Labelled ()
        fromAssocList showLabel ass = Labelled labelling graph
            where
                graph = buildG' edges
                (labelling, edges) = mkLabel showLabel tt
                tt = [(v,(),t) | (v, cs) <- ass, t <- cs]

