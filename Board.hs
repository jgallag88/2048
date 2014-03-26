--Version of 2048 game
{-# LANGUAGE TupleSections #-}

module Board (BoardT (..),
              Board,
              Index (..),
              Row,
              Col,
              Square (..),
              Block (..),
              emptyBoard,
              initBoardT,
              getSquare,
              setSquare,
              getEmpties,
              getSquares,
              showBoard) where

import Data.Array

data BoardT a = BoardT (Array Row (Array Col a))
    deriving (Eq)

type Board = BoardT Square

instance Show a => Show (BoardT a) where
    show = showBoard

--Type for indices of the board
data Index = I0 | I1 | I2 | I3
            deriving (Show, Eq, Ord, Bounded, Enum, Ix)
    
type Row = Index
type Col = Index

data Square = Full Block | Empty
              deriving (Eq)

instance Show Square where 
    show = showSquare

data Block = B2
           | B4
           | B8
           | B16
           | B32
           | B64
           | B128
           | B256
           | B512
           | B1024
           | B2048
           deriving (Show, Eq, Ord, Enum, Bounded)

emptyBoard :: Board
emptyBoard = initBoardT Empty

initBoardT :: a -> BoardT a
initBoardT val = BoardT $ array (minBound, maxBound) $
                           map (,mkRow) [minBound .. maxBound]
    where 
       mkRow = array (minBound, maxBound) $ 
                     map (,val) [minBound .. maxBound]

getSquare :: Row -> Col -> BoardT a -> a
getSquare row col (BoardT board) =
    board ! row ! col 

setSquare :: Row -> Col -> a -> BoardT a -> BoardT a
setSquare row col sqr (BoardT board) =
    BoardT $ board // [(row, newRow)]
    where newRow = board ! row // [(col, sqr)]

--Produce list of indices of empty squares
getEmpties :: Board -> [(Row, Col)]
getEmpties board = fmap dropSqr $ filter isEmpty $ getSquares board
    where isEmpty (_, _, Empty) = True
          isEmpty _             = False
          dropSqr (row, col, _) = (row, col)

getSquares :: BoardT a -> [(Row, Col, a)]
getSquares (BoardT b) =
    let (rows, colArrs) = unzip $ assocs b
        (cols, squares) = unzip $ concatMap assocs colArrs
        rows' = concatMap (replicate $ length cols `div` length rows) rows
    in zip3 rows' cols squares

--Pretty printer
showBoard :: Show a => BoardT a -> String
showBoard board =  
    foldl mkRow borderRow indexRange   
    where 
       mkRow acc row = acc ++ 
                       concat (replicate botBlankLns blankLine) ++ 
                       textLine ++ "\n" ++
                       concat (replicate topBlankLns blankLine) ++ 
                       borderRow
            where textLine = foldl mkBox "|" indexRange
                  mkBox acc' col = acc'
                                   ++ padCenter width (show $ getSquare row col board)
                                   ++ "|"
                  blankLine = "|" 
                              ++ concat (replicate rows (replicate width ' ' ++ "|"))
                              ++ "\n"
                  botBlankLns = (height - 1) `div` 2
                  topBlankLns = height - botBlankLns - 1
       borderRow = replicate ((width+1)*rows + 1) '-' ++ "\n"
       indexRange = [minBound .. maxBound] :: [Index]
       rows = length indexRange
       height = 3
       width = 8

padCenter :: Int -> String -> String
padCenter len str = left ++ str ++ right
    where
       left = replicate ((len - length str) `div` 2) ' '
       right = replicate (len - length str - length left) ' '

showSquare :: Square -> String
showSquare Empty = ""
showSquare (Full b) = tail . show $ b
