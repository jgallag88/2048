module Game where

import Prelude hiding (Left, Right)
import System.Random
import System.IO
import System.Console.ANSI
import Board

data Game = Game GameState Board StdGen

data GameState = Win | Loss | Ongoing

data Move = Left | Right | Up | Down
            deriving (Show)

playGame :: IO ()
playGame = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    gen <- getStdGen
    play $ newGame gen
    where play game@(Game _ board gen') = do
            clearScreen
            print board
            dir <- getMove
            let game'@(Game state board' _) = move dir game
            case state of
              Win -> print "You win!"
              Loss -> print "You lose."
              Ongoing -> play game'

getMove :: IO Move
getMove = getChar >>= parseChar
    where parseChar c =
             case c of
               'a' -> return Left
               's' -> return Down
               'd' -> return Right
               'w' -> return Up
               _   -> getMove

newGame :: StdGen -> Game
newGame gen = Game Ongoing board gen'
    where (board, gen') = addRandBlock emptyBoard gen

--Updates board after a turn. Moves squares according to user Move, and then adds
-- a random new square
move :: Move -> Game -> Game
move dir (Game _ board gen) =
    let board' = push dir board
    in case gameState board' of
         Win -> Game Win board' gen
         Loss -> Game Loss board' gen
         Ongoing -> if board == board'
                    then Game Ongoing board gen
                    else uncurry (Game Ongoing) $ addRandBlock board' gen

gameState :: Board -> GameState
gameState board
    | win board  = Win
    | loss board = Loss
    | otherwise  = Ongoing

win :: Board -> Bool
win board = not . null $ filter is2048 $ getSquares board
    where is2048 (_, _, Full B2048) = True
          is2048 _                  = False

loss :: Board -> Bool
loss board = board == push Left board &&
             board == push Right board &&
             board == push Up board &&
             board == push Down board

-- Add a block in a random empty square
addRandBlock :: Board -> StdGen -> (Board, StdGen)
addRandBlock board gen = 
    let ((row, col), gen') = listRand gen $ getEmpties board
        -- ^ indices of random empty square
        (newBlk, gen'') = listRand gen' [B2, B4]
        -- ^ Random block to put in the empty square
        board' = setSquare row col (Full newBlk) board
    in (board', gen'')

--Random element from list. O(n).
listRand :: StdGen -> [a] -> (a, StdGen)
listRand gen xs = (xs !! index, gen')
    where (index, gen') = randomR (0, length xs - 1) gen

push :: Move -> Board -> Board
push dir board = foldr (pushRow dir) board [minBound .. maxBound]

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

