--Version of 2048 game
{-# LANGUAGE TupleSections #-}

module Board (Board (..),
              Index (..),
              Row,
              Col,
              Square (..),
              Block (..),
              emptyBoard,
              getSquare,
              setSquare,
              getEmpties,
              getSquares,
              showBoard) where

import Data.Array

data Board = Board (Array Row (Array Col Square))
    deriving (Eq)

instance Show Board where
    show = showBoard

--Type for indices of the board
data Index = I0 | I1 | I2 | I3
            deriving (Show, Eq, Ord, Bounded, Enum, Ix)
    
type Row = Index
type Col = Index

data Square = Full Block | Empty
              deriving (Eq, Show)

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
emptyBoard = Board $ array (minBound, maxBound) $
                           map (,mkRow) [minBound .. maxBound]
    where 
       mkRow = array (minBound, maxBound) $ 
                     map (,Empty) [minBound .. maxBound]

getSquare :: Row -> Col -> Board -> Square
getSquare row col (Board board) =
    board ! row ! col 

setSquare :: Row -> Col -> Square -> Board -> Board
setSquare row col sqr (Board board) =
    Board $ board // [(row, newRow)]
    where newRow = board ! row // [(col, sqr)]

--Produce list of indices of empty squares
getEmpties :: Board -> [(Row, Col)]
getEmpties board = fmap dropSqr $ filter isEmpty $ getSquares board
    where isEmpty (_, _, Empty) = True
          isEmpty _             = False
          dropSqr (row, col, _) = (row, col)

getSquares :: Board -> [(Row, Col, Square)]
getSquares (Board b) =
    let (rows, colArrs) = unzip $ assocs b
        (cols, squares) = unzip $ concatMap assocs colArrs
        rows' = concatMap (replicate $ length cols `div` length rows) rows
    in zip3 rows' cols squares

--Pretty printer
showBoard :: Board -> String
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
                                   ++ padCenter width (showSquare $ getSquare row col board)
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
