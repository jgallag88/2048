{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (Left, Right)
import Game 
import Board
import Test.QuickCheck
import Control.Applicative
import Data.Array

instance Arbitrary Block where 
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary Square where
    arbitrary = oneof [fmap Full arbitrary, pure Empty]

instance Arbitrary Board where
    arbitrary = foldl set (pure emptyBoard) $ (,, validSqr) <$> indices <*> indices 
        where set genBoard (row, col, genSquare) =
                setSquare row col <$> genSquare <*> genBoard
              indices = [minBound .. maxBound]
              validSqr = arbitrary `suchThat` ( /= Full maxBound) --Max value can't be combined

instance Arbitrary Move where
    arbitrary = elements [Left, Right, Up, Down]
 
sameSum board dir = getSum board == getSum (push dir board)

getSum (BoardT board) = sum squares
    where squares = map getIntVal $ concatMap elems $ elems board
          getIntVal Empty    = 0
          getIntVal (Full s) = 2 ^ (1 + fromEnum s) 

main = quickCheck sameSum
