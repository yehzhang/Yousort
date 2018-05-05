module Test.Tournament
    ( tournamentSpecs
    ) where

import Data.Foldable (and)
import Data.List (List(Nil), sort, (:))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust)
import Data.Tuple (Tuple(..), uncurry)
import Partial.Unsafe (unsafePartial)
import Prelude
import Test.QuickCheck ((/=?), (==?))
import Test.Spec (describe, it, Spec)
import Test.Spec.Assertions (shouldEqual)

import Data.Tournament (GameResult(Right, Left), Tournament, areExhaustive, fromFoldable, ranked, next, play, unplay)
import Data.Tournament.Encoding2 (nextCombination, prevCombination)
import Data.Tournament.Encoding2.Cantor (depair, pair)
import Data.Tournament.Prelude (MultitonList(MultitonList))
import Test.QuickCheck.Class (quickCheck)
import Test.Types (DoubletonList(..), LargeNat(..), MultiComboList(..), Nat(..), Negative(..), SingleComboList(..), SingletonList(..), SmallNat(..), TripletonList(..))


tournamentSpecs :: Spec Unit
tournamentSpecs = do
    cantorSpecs
    encoding2Specs
    tournamentSpecs'

cantorSpecs :: Spec Unit
cantorSpecs = describe "Module Data.Tournament.Encoding2.Cantor" do
    describe "pair" do
        it "should accept zeros" do
            pair 0 0 `shouldEqual` Just 0

        it "should reject a negative number" do
            quickCheck \(Negative x) (SmallNat y) -> pair x y ==? Nothing
            quickCheck \(SmallNat x) (Negative y) -> pair x y ==? Nothing

        it "should reject a large natrual number" do
            quickCheck \(LargeNat x) (SmallNat y) -> pair x y ==? Nothing
            quickCheck \(SmallNat x) (LargeNat y) -> pair x y ==? Nothing

    describe "depair" do
        it "should accept zero" do
            depair 0 `shouldEqual` Just (Tuple 0 0)

        it "should reject a negative number" do
            quickCheck \(Negative z) -> depair z ==? Nothing

        it "should accept a large natrual number" do
            quickCheck \(LargeNat z) -> isJust (depair z) ==? true

    it "should encode and decode" do
        quickCheck \(Nat x) -> (depair x >>= uncurry pair) ==? Just x
        quickCheck \(SmallNat x) (SmallNat y) ->
            (pair x y >>= depair) ==? Just (Tuple x y)

encoding2Specs :: Spec Unit
encoding2Specs = describe "Module Data.Tournament.Encoding2" do
    describe "nextCombination" do
        it "should be a no-op on SingleComboList" do
            quickCheck \(SingleComboList xs) (i :: Int) ->
                nextCombination i xs ==? xs

        it "should not be a no-op on MultiComboList" do
            quickCheck \(MultiComboList xs) (i :: Int) ->
                nextCombination i xs /=? xs

        it "should not be a no-op on MultiComboList initially" do
            quickCheck \(MultiComboList xs) -> nextCombination 0 xs /=? xs

    describe "prevCombination" do
        it "should be a no-op on SingleComboList" do
            quickCheck \(SingleComboList xs) (i :: Int) ->
                prevCombination i xs ==? xs

        it "should not be a no-op on MultiComboList" do
            quickCheck \(MultiComboList xs) (i :: Int) ->
                prevCombination i xs /=? xs

    it "should encode and decode" do
        quickCheck \(xs :: List Int) (i :: Int) ->
            prevCombination (i + 1) (nextCombination i xs) ==? xs
        quickCheck \(xs :: List Int) (i :: Int) ->
            nextCombination (i - 1) (prevCombination i xs) ==? xs

tournamentSpecs' :: Spec Unit
tournamentSpecs' = describe "Module Data.Tournament" do
    describe "Conversion" do
        it "should start with the only value ranked over a SingletonList" do
            quickCheck \(SingletonList xs :: SingletonList Int) ->
                (ranked $ fromFoldable xs) ==? xs

        it "should not start with any ranked over a MultitonList" do
            quickCheck \(MultitonList xs :: MultitonList Int) ->
                (ranked $ fromFoldable xs) ==? Nil

        it "should not start with any next over a SingletonList" do
            quickCheck \(SingletonList xs :: SingletonList Int) ->
                (next $ fromFoldable xs) ==? Nothing

        it "should start with the first two values next over a MultitonList" do
            quickCheck $ unsafePartial $
                \(MultitonList xs@(x:x':_) :: MultitonList Int) ->
                    (next $ fromFoldable xs) ==? Just (Tuple x x')

    describe "areExhaustive" do
        it "should be true over a singleton list" do
            quickCheck \(SingletonList xs :: SingletonList Int) ->
                (areExhaustive 0 $ fromFoldable xs) ==? true

        it "should be true over a doubleton list" do
            quickCheck \(DoubletonList xs :: DoubletonList Int) ->
                (areExhaustive 0 $ fromFoldable xs) ==? true

        it "should be correct over a tripleton list" do
            quickCheck \(TripletonList xs :: TripletonList Int) ->
                (areExhaustive 0 $ fromFoldable xs) ==? false
            quickCheck \(TripletonList xs :: TripletonList Int) ->
                (areExhaustive 1 $ fromFoldable xs) ==? false
            quickCheck \(TripletonList xs :: TripletonList Int) ->
                (areExhaustive 2 $ fromFoldable xs) ==? true

    describe "play" do
        it "should not be a no-op on a new tournament" do
            quickCheck \(t :: Tournament Int) (r :: GameResult) ->
                play r t /= Nothing

        it "should eventually sort a list" do
            quickCheck \(xs :: List Int) -> tournamentSort xs ==? sort xs

    describe "unplay" do
        it "should cancel the first play" do
            quickCheck \(t :: Tournament Int) (r :: GameResult) ->
                (play r t >>= unplay r) == Just t

        it "should cancel each following play" do
            quickCheck \(t :: Tournament Int) rs ->
                and $ uncurry eq <$> (reciprocateUntilEnd rs t)

tournamentSort :: forall a. Ord a => List a -> List a
tournamentSort l = fromFoldable l # playUntilEnd # ranked

playUntilEnd :: forall a. Ord a => Tournament a -> Tournament a
playUntilEnd t = next t
    <#> uncurry compare
    <#> toGameResult
    >>= flip play t
    <#> playUntilEnd
    # fromMaybe t
  where
    toGameResult LT = Right
    toGameResult EQ = Right
    toGameResult GT = Left

reciprocateUntilEnd :: forall a. Eq a
    => List GameResult
    -> Tournament a
    -> List (Tuple (Maybe (Tournament a)) (Maybe (Tournament a)))
reciprocateUntilEnd (r:rs) t
    | isJust $ next t   = case play r t of
        Just t' -> Tuple (Just t) (unplay r t') : (reciprocateUntilEnd rs t')
        Nothing -> Nil
reciprocateUntilEnd _ _ = Nil
