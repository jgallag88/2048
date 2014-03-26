--An AI for the 2048 game

module AI where

import Board
import Control.Applicative

type Value = Int

evaluate :: Board -> Value
evaluate board = sum $ [] <*> pure board

--Heuristic to value boards which place large value toward corners
--Basically we create a (BoardT Int) where each square has the value 
-- of the block in that square*
--edgeWeight :: Board -> Value
edgeWeight board = (*) <$> edgeKernel <*> fmap squareVal board

edgeKernel :: BoardT Int
edgeKernel = initBoardTList [100,  10,  10, 100,
                              10,   0,   0,  10,
                              10,   0,   0,  10,
                             100,  10,  10, 100]
