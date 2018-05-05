{-|
State of a tournament and its operations, adapted from
[Tournament sort](https://en.wikipedia.org/wiki/Tournament_sort).

What game is played does not matter to the tournament. The tournament can
proceed with the mere knowledge of @GameResult@.

Theoretically, more than two players can be in one game, but comparing them is
not friendly to comparators, both user and programmer. Therefore, @next@ only
returns two players.
-}
module Data.Tournament
    ( -- * Types
      GameResult(..)
    , Tournament
    , Holds

      -- * Conversion
    , fromFoldable
    , ranked
    , next

      -- * Transition
    , play
    , hold
    , areExhaustive
    , unplay
    , unhold
    ) where

import Control.Comonad.Cofree (Cofree, head, tail, (:<))
import Data.Foldable (class Foldable, foldl, foldr, foldrDefault, foldMapDefaultL)
import Data.Generic.Rep (class Generic)
import Data.List (List(Nil), length, singleton, snoc, uncons, unsnoc, (:), null)
import Data.List (fromFoldable) as L
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Tuple (Tuple(..), uncurry)
import Prelude
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, elements)

import Data.Tournament.Encoding2 (nextCombination, prevCombination)
import Data.Tournament.Prelude (choose, fromCofree, MultitonList)


type Player a = Cofree List a
type Holds = Int  -- ^ number of games that have been held in a row

newtype Tournament a = Tournament
    { ranked :: List (Player a)
    , unranked :: List (Player a)
    }

derive instance genericTournament :: Generic (Tournament a) _

derive instance eqTournament :: (Eq a) => Eq (Tournament a)

instance foldableTournament :: Foldable Tournament where
    foldl f acc (Tournament t) = foldr (flip f) acc (head <$> t.ranked)
        # \acc' -> foldl (foldl f) acc' t.unranked
    foldMap = foldMapDefaultL
    foldr = \x -> foldrDefault x  -- to suppress compiler error

data GameResult = Left | Right

derive instance eqGameResult :: Eq GameResult

-- | Returns a tournament that guarantees next players.
fromFoldable :: forall f. Foldable f => f ~> Tournament
fromFoldable xs = Tournament $ case pure <$> L.fromFoldable xs of
    ps | length ps >= 2 -> { ranked: Nil
                           , unranked: ps
                           }
    ps                  -> { ranked: ps
                           , unranked: Nil
                           }

-- | Returns a list where the higher ranked is closer to the tail.
ranked :: Tournament ~> List
ranked (Tournament t) = head <$> t.ranked

next :: forall a. Tournament a -> Maybe (Tuple a a)
next (Tournament { unranked: p:p':_ }) = Just $ Tuple (head p) (head p')
next _                                 = Nothing

-- | Transitions a tournament to the next state according to result of a game.
play :: forall a. GameResult -> Tournament a -> Maybe (Tournament a)
play r (Tournament t) = Tournament <$> rankify <$> playNext t
  where
    playNext t'@{ unranked: p:p':ps } = Just $
        t' { unranked = snoc ps $ play' r p p' }
    playNext _                        = Nothing

    play' Left  = promote
    play' Right = flip promote

    rankify t'@{ unranked: p:Nil } = rankify $ t' { ranked = p : t'.ranked
                                                  , unranked = tail p
                                                  }
    rankify t'                     = t'

hold :: forall a. Holds -> Tournament a -> Tournament a
hold h (Tournament t) = Tournament $
    t { unranked = nextCombination h t.unranked }

areExhaustive :: forall a. Holds -> Tournament a -> Boolean
areExhaustive h (Tournament t) = h >= length t.unranked `choose` 2 - 1

unplay :: forall a. GameResult -> Tournament a -> Maybe (Tournament a)
unplay r (Tournament t) = Tournament <$> (unplayLast =<< unrankify t)
  where
    unrankify t'@{ ranked: p:ps }
        -- when there is exactly one value in the tournament
        | null ps && null t'.unranked           = Nothing
        | length (tail p) == length t'.unranked = unrankify $
            t' { ranked = ps
               , unranked = singleton p
               }
    unrankify t'                                = Just t'

    unplayLast t'@{ unranked: ur } = case unsnoc ur of
        Just { init: ps, last: p } ->
            demote p <#> \wl -> t' { unranked = uncurry (unplay' r) wl ps }
        _                          -> Nothing

    unplay' Left w l ps = w:l:ps
    unplay' Right w l ps = l:w:ps

unhold :: forall a. Holds -> Tournament a -> Tournament a
unhold h (Tournament t) = Tournament $
    t { unranked = prevCombination h t.unranked }

promote :: forall a. Player a  -- ^ winner
                  -> Player a  -- ^ loser
                  -> Player a  -- ^ winner, whose children include the loser
promote w l = fromCofree (\a ps -> a :< l:ps) w

demote :: forall a.
       Player a
    -> Maybe (Tuple (Player a) (Player a))  -- ^ Maybe (winner, loser)
demote = fromCofree \a ps -> uncons ps <#> \r -> Tuple (a :< r.tail) r.head


instance arbitraryTournament :: Arbitrary a => Arbitrary (Tournament a) where
    arbitrary :: Gen (Tournament a)
    arbitrary = fromFoldable <<< unwrap <$> (arbitrary :: Gen (MultitonList a))

instance arbitraryGameResult :: Arbitrary GameResult where
    arbitrary :: Gen GameResult
    arbitrary = elements (Left :| [Right])
