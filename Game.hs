module Game where

import Prelude hiding (Left, Right)
import Board

data Move = Left | Right | Up | Down

move :: Move -> Board -> Board
move dir board = foldr (pushRow dir) board [minBound .. maxBound]

--Fold a single row
pushRow :: Move -> Row -> Board -> Board
pushRow Left row  = pushRow' row minBound (succ minBound) succ
pushRow Right row = pushRow' row maxBound (pred maxBound) pred

pushRow' :: Row ->
            -- ^ Particular row which we are compressing
            Col -> 
            -- ^ Col of square which we are now moving
            Col -> 
            -- ^ Col of square to which we are considering moving it
            (Col -> Col) ->
            -- ^ pred or succ. Governs which way we proceed, Left or Right 
            Board -> 
            -- ^ Input board
            Board
            -- ^ Adjusted board
pushRow' row endCol startCol next board =
    let (board', endCol') = go (getSquare row startCol board)
                               (getSquare row endCol board)
    in if startCol == maxBound || startCol == minBound
       then board'
       else pushRow' row endCol' (next startCol) next board'

    where go Empty _               = (board, endCol)
          -- ^ No block here to move
          go (Full srcBlock) Empty = (setSquare row startCol Empty $
                                      setSquare row endCol (Full srcBlock) board,
                                      endCol)
          -- ^ Destination is empty, so move block there
          go (Full srcBlock) (Full dstBlock)
                | srcBlock == dstBlock = (setSquare row endCol (Full $ succ srcBlock) $
                                          setSquare row startCol Empty board,
                                          next endCol)
                -- ^ Block is equal to block in destination, so merge them
                | otherwise            = (setSquare row (next endCol) (Full srcBlock) $
                                          setSquare row startCol Empty board,
                                          next endCol)
                -- ^ Block is not equal to destination block, so place it right after
