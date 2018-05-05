module Data.Tournament.Encoding2
    ( nextCombination
    , prevCombination
    ) where

import Data.Foldable (length)
import Data.List (List, updateAt, (!!))
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Prelude

import Data.Tournament.Encoding2.Cantor (depair)
import Data.Tournament.Prelude (choose, positiveMod)


nextCombination :: forall a. Int -> List a -> List a
nextCombination n xs =
    applyNthCombination n xs >>= applyNthCombination (n + 1) # fromMaybe xs

prevCombination :: forall a. Int -> List a -> List a
prevCombination n xs =
    applyNthCombination n xs >>= applyNthCombination (n - 1) # fromMaybe xs

applyNthCombination :: forall a. Int -> List a -> Maybe (List a)
applyNthCombination n xs = depair n' >>= \(Tuple x y) -> swap x (x + 1 + y) xs
  where
    n' = n `positiveMod` (length xs `choose` 2)

swap :: forall a. Int -> Int -> List a -> Maybe (List a)
swap i j xs = do
    x <- xs !! i
    y <- xs !! j
    updateAt i y xs >>= updateAt j x
