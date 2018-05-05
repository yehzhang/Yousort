module Data.Tournament.Prelude
    ( -- * Math
      factorial
    , choose
    , positiveMod

      -- * Cofree
    , fromCofree

      -- * Arbitrary
    , MultitonList(..)
    ) where

import Control.Apply (lift2)
import Control.Comonad.Cofree (Cofree, head, tail)
import Data.Array (range)
import Data.Foldable (product)
import Data.Int (toNumber, floor)
import Data.List (List)
import Data.Newtype (class Newtype)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, listOf)


-- | Returns `0` for invalid input.
factorial :: Int -> Number
factorial 0 = 1.0
factorial n = product $ toNumber <$> range 1 n

choose :: Int -> Int -> Int
choose n r = floor $ factorial n / (factorial r * factorial (n - r))

fromCofree :: forall f a b. (a -> f (Cofree f a) -> b) -> Cofree f a -> b
fromCofree f c = f (head c) (tail c)

positiveMod :: Int -> Int -> Int
positiveMod a b = (a `mod` b + b) `mod` b

newtype MultitonList a = MultitonList (List a)

derive instance newtypeMultitonList :: Newtype (MultitonList a) _

instance arbitraryMultitonList :: (Arbitrary a) => Arbitrary (MultitonList a)
  where
    arbitrary :: Gen (MultitonList a)
    arbitrary = MultitonList <$> lift2 append (listOf 2 arbitrary) arbitrary
