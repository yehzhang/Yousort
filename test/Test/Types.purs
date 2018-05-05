module Test.Types
    ( Nat(..)
    , SmallNat(..)
    , LargeNat(..)
    , Negative(..)
    , SingleComboList(..)
    , MultiComboList(..)
    , SingletonList(..)
    , DoubletonList(..)
    , TripletonList(..)
    ) where

import Data.Foldable (length)
import Data.List (List, nub, take)
import Data.Ord (abs)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, listOf, suchThat)


newtype Nat = Nat Int

derive newtype instance eqNat :: Eq Nat

derive newtype instance showNat :: Show Nat

instance arbitraryNat :: Arbitrary Nat where
    arbitrary :: Gen Nat
    arbitrary = Nat <<< abs <$> arbitrary

newtype SmallNat = SmallNat Int

derive newtype instance eqSmallNat :: Eq SmallNat

derive newtype instance showSmallNat :: Show SmallNat

instance arbitrarySmallNat :: Arbitrary SmallNat where
    arbitrary :: Gen SmallNat
    arbitrary = SmallNat <$> chooseInt 0 99

newtype LargeNat = LargeNat Int

derive newtype instance eqLargeNat :: Eq LargeNat

derive newtype instance showLargeNat :: Show LargeNat

instance arbitraryLargeNat :: Arbitrary LargeNat where
    arbitrary :: Gen LargeNat
    arbitrary = LargeNat <$> chooseInt (top / 10 * 9) top

newtype Negative = Negative Int

derive newtype instance eqNegative :: Eq Negative

derive newtype instance showNegative :: Show Negative

instance arbitraryNegative :: Arbitrary Negative where
    arbitrary :: Gen Negative
    arbitrary = Negative <<< (\n -> -n - 1) <<< abs <$> arbitrary

newtype SingleComboList = SingleComboList (List SmallNat)

instance arbitrarySingleComboList :: Arbitrary SingleComboList where
    arbitrary :: Gen SingleComboList
    arbitrary = SingleComboList <<< take 2 <<< nub <$> arbitrary

newtype MultiComboList = MultiComboList (List SmallNat)

instance arbitraryMulticomboList :: Arbitrary MultiComboList where
    arbitrary :: Gen MultiComboList
    arbitrary = MultiComboList <$> arbitrary `suchThat`
        \xs -> length xs >= 3 && nub xs == xs

newtype SingletonList a = SingletonList (List a)

instance arbitrarySingletonList :: (Arbitrary a) => Arbitrary (SingletonList a)
  where
    arbitrary :: Gen (SingletonList a)
    arbitrary = SingletonList <$> listOf 1 arbitrary

newtype DoubletonList a = DoubletonList (List a)

instance arbitraryDoubletonList :: (Arbitrary a) => Arbitrary (DoubletonList a)
  where
    arbitrary :: Gen (DoubletonList a)
    arbitrary = DoubletonList <$> listOf 2 arbitrary

newtype TripletonList a = TripletonList (List a)

instance arbitraryTripletonList :: (Arbitrary a) => Arbitrary (TripletonList a)
  where
    arbitrary :: Gen (TripletonList a)
    arbitrary = TripletonList <$> listOf 3 arbitrary
