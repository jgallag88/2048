module Game where

import Prelude hiding (Left, Right)
import Board

data Move = Left | Right | Up | Down
            deriving (Show)

move :: Move -> Board -> Board
move dir board = foldr (pushRow dir) board [minBound .. maxBound]

--Fold a single row
pushRow :: Move -> Row -> Board -> Board
pushRow dir row board =
    case dir of
      Left  -> pushRow' row minBound (succ minBound) succ getSquare setSquare board
      Right -> pushRow' row maxBound (pred maxBound) pred getSquare setSquare board
      Down  -> pushRow' row maxBound (pred maxBound) pred transGet  transSet  board
      Up    -> pushRow' row minBound (succ minBound) succ transGet  transSet  board
    where transGet row col = getSquare col row
          transSet row col = setSquare col row 

pushRow' :: Row ->
            -- ^ Particular row which we are compressing
            Col -> 
            -- ^ Col of square which we are now moving
            Col -> 
            -- ^ Col of square to which we are considering moving it
            (Col -> Col) ->
            -- ^ pred or succ. Governs which way we proceed, Left or Right 
            (Index -> Index -> Board -> Square) ->

            (Index -> Index -> Square -> Board -> Board) ->
            Board -> 
            -- ^ Input board
            Board
            -- ^ Adjusted board
pushRow' row endCol startCol next get set board =
    let (board', endCol') = go (get row startCol board)
                               (get row endCol board)
    in if startCol == maxBound || startCol == minBound
       then board'
       else pushRow' row endCol' (next startCol) next get set board'

    where go Empty _               = (board, endCol)
          -- ^ No block here to move
          go (Full srcBlock) Empty = (set row startCol Empty $
                                      set row endCol (Full srcBlock) board,
                                      endCol)
          -- ^ Destination is empty, so move block there
          go (Full srcBlock) (Full dstBlock)
                | srcBlock == dstBlock = (set row endCol (Full $ succ srcBlock) $
                                          set row startCol Empty board,
                                          next endCol)
                -- ^ Block is equal to block in destination, so merge them
                | otherwise            = (set row (next endCol) (Full srcBlock) $
                                          set row startCol Empty board,
                                          next endCol)
                -- ^ Block is not equal to destination block, so place it right after

