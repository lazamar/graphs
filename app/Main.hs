module Main where

import Control.Monad (guard)
import Control.Monad.ST
import Data.Array (Array, array, (!))
import Data.Bifunctor (bimap, second)
import Data.Heap (Heap)
import Data.List (minimumBy, nub, sortBy)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.STRef
import Data.Set (Set)
import Data.Traversable (for)
import Data.Tuple (swap)
import Debug.Trace
import Graph
import qualified Data.Array as Array
import qualified Data.Heap as Heap
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main =
    -- putStrLn $ visualiseGraph Nothing False $ Labelled kwLabel kwGraph
    print $
        fmap (map kwLabel) $
            shortestPath' (kwVertex "a") (kwVertex "i") kwGraph
    where
        Labelled (kwLabel , kwVertex) kwGraph =
            buildWithLabels
                [ ("a" , 1 , "g")
                , ("a" , 1 , "j")
                , ("g" , 5 , "b")
                , ("g" , 10 , "f")
                , ("b" , 1 , "i")
                , ("b" , 1 , "a")
                , ("c" , 1 , "e")
                , ("c" , 1 , "h")
                , ("e" , 1 , "j")
                , ("e" , 1 , "h")
                , ("e" , 1 , "d")
                , ("f" , 1 , "i")
                ]

        Labelled kingLabel kingGraph =
            fromAssocList
                pure
                [ ('a' , ['j' , 'g'])
                , ('b' , ['a' , 'i'])
                , ('c' , ['h' , 'e'])
                , ('e' , ['d' , 'h' , 'j'])
                , ('f' , ['i'])
                , ('g' , ['f' , 'b'])
                ]

        weightedGraph =
            buildG
                (1 , 5)
                [ (1 , 1 , 2)
                , (2 , 2 , 3)
                , (2 , 2 , 5)
                , (3 , 3 , 4)
                , (4 , 4 , 5)
                ]

        graph =
            buildG (1 , 8) . nub . map ordered $
                [ (1 , 0 , 2)
                , (1 , 0 , 3)
                , (1 , 0 , 4)
                , (2 , 0 , 1)
                , (2 , 0 , 6)
                , (3 , 0 , 1)
                , (3 , 0 , 4)
                , (3 , 0 , 5)
                , (3 , 0 , 6)
                , (4 , 0 , 1)
                , (4 , 0 , 3)
                , (4 , 0 , 5)
                , (5 , 0 , 3)
                , (5 , 0 , 4)
                , (5 , 0 , 6)
                , (5 , 0 , 7)
                , (5 , 0 , 8)
                , (6 , 0 , 2)
                , (6 , 0 , 3)
                , (6 , 0 , 5)
                , (6 , 0 , 7)
                , (7 , 0 , 5)
                , (7 , 0 , 6)
                , (7 , 2 , 8)
                , (8 , 0 , 4)
                , (8 , 0 , 5)
                , (8 , 2 , 7)
                ]
        ordered (v , e , t) = if v > t then (v , e , t) else (t , e , v)

        -- Displaying
        visualiseGraph
            mVertices --  Subset of vertices to include
            undirected -- If True no arrow points are shown
            (Labelled (lab , _) rawG) =
                unlines
                    [ "digraph name {"
                    , "rankdir=LR;"
                    , unlines $ map showNode $ fromMaybe (vertices graph) mVertices
                    , unlines $ map showEdge $ edges graph
                    , "}"
                    ]
                where
                    graph =
                        if undirected
                            then buildG' $ nub $ map ordered $ edges rawG
                            else rawG

                    ordered (v , e , t) = if v > t then (v , e , t) else (t , e , v)

                    showEdge (from , t , to) =
                        unwords
                            [ show from
                            , "->"
                            , show to
                            , "["
                            , "label = \"" ++ show t ++ "\""
                            , if undirected then ", arrowhead=none" else ""
                            , "];"
                            ]
                    showNode v = unwords [show v , "[ label = " ++ (show $ lab v) , "];"]

        visualiseForest :: Labelling -> Forest -> String
        visualiseForest l forest =
            visualiseGraph (Just $ fvertices forest) False $
                Labelled l $
                    buildG' $
                        fedges forest

        fvertices :: Forest -> [Vertex]
        fvertices [] = []
        fvertices (Node v children : siblings) = v : fvertices siblings

        fedges :: Forest -> [Edge ()]
        fedges = flip go []
            where
                go [] = id
                go (Node v children : siblings) =
                    (++ [(v , () , c) | Node c _ <- children])
                        . go children
                        . go siblings

        fromAssocList :: Ord a => (a -> String) -> [(a , [a])] -> Labelled ()
        fromAssocList toLabel ass =
            buildWithLabels
                [ (toLabel from , () , toLabel to)
                | (from , succ) <- ass
                , to <- succ
                ]
