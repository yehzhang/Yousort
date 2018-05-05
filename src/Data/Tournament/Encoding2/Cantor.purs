module Data.Tournament.Encoding2.Cantor
    ( pair
    , depair
    ) where

import Data.Int (floor, pow, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Math (sqrt)
import Prelude


-- | Decodes an integer as two using Cantor pairing function.
depair :: Int -> Maybe (Tuple Int Int)
depair z
    | z < 0     = Nothing
    | otherwise = Just $ Tuple x y
      where
        w = floor $ (sqrt (8.0 * toNumber z + 1.0) - 1.0) / 2.0
        t = (pow w 2 + w) / 2
        y = z - t
        x = w - y

-- | Encodes two integers as one using Cantor pairing function.
pair :: Int -> Int -> Maybe Int
pair x y = depair z >>= case _ of
    Tuple x' y' | x' == x && y' == y -> Just z
    _                                -> Nothing
  where
    nx = toNumber x
    ny = toNumber y
    z = floor $ (nx + ny) * (nx + ny + 1.0) / 2.0 + ny
