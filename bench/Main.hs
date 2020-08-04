module Main where

import Criterion.Main
import Data.Char (chr, ord)
import Graph

-- | Create a graph with `size` nodes and size/4 edges.
randomG size =
    buildWithLabels
        . keepEveryOther 4
        . ( \labels ->
                [ (from , (`mod` 10) $ sum $ map ord $ from <> to , to)
                | from <- labels
                , to <- labels
                ]
          )
        $ map (pure . chr) [65 .. 65 + size]

keepEveryOther factor list =
    [ val
    | (ix , val) <- zip [0 ..] list
    , ix `mod` factor /= 0
    ]

allPathsFrom from size getPath = getPath from g
    where
        Labelled _ g = randomG size

-- Our benchmark harness.
main =
    defaultMain
        [ --  bgroup
          --        "10 nodes"
          --        [ bench "plain" $ nf (allPathsFrom 0 10) allShortestPathsFrom
          --        , bench "loop " $ nf (allPathsFrom 0 10) allShortestPathsFrom'
          --        ]
          --  , bgroup
          --        "100 nodes"
          --        [ bench "plain" $ nf (allPathsFrom 0 100) allShortestPathsFrom
          --        , bench "loop " $ nf (allPathsFrom 0 100) allShortestPathsFrom'
          --        ]
          bgroup
              "1000 nodes"
              [ bench "loop " $ nf (allPathsFrom 0 1000) allShortestPathsFrom'
              ]
              -- bgroup
              --     "loop"
              --     [ bench (show size) $ nf (allPathsFrom 0 size) allShortestPathsFrom'
              --     | size <- [1 , 10 .. 200]
              --     ]
        ]
