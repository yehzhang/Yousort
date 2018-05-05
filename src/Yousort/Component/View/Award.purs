module Yousort.Component.View.Award
    ( award
    ) where

import Data.Foldable (null)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..), drop, reverse, take, (:))
import Materialize.Cards (card, hoverable)
import Materialize.Grid (column, m1, m2, m4, m5, m8, offset, s12, s6, xl2, xl3, xl4, xl6)
import Materialize.Markup (classList, (~))
import Prelude hiding (div)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (empty, (!))

import Data.Tournament (ranked)
import Yousort.Component.State (Comparable, State)
import Yousort.Component.Update (Event)
import Yousort.Component.View.Common (comparableCardMain, comparableCardBase, description, monologue, navigation, row, section, sectionHeader, sectionRow)


award :: State -> HTML Event
award s@{ tournament: t } = do
    navigation
    section case ranked t of
        Nil -> description "Nothing is sorted yet."
        cs  -> standingCards 0 1 $ reverse cs

standingCards :: Int -> Int -> (List Comparable) -> HTML Event
standingCards _ _ Nil = empty
standingCards 0 s (c:cs) = do
    sectionRow $ prominentStandingCard s c
        ! classList (column s12 ~ m8 ~ offset m2 ~ xl6 ~ offset xl3)

    when (null cs) $ section $
        description "I am the only entry here, but I am also the best entry."

    standingCards 1 (s + 1) cs
standingCards 1 s (c:cs) = do
    section do
        row do
            standingCard s c
                ! classList (column s6 ~ m5 ~ offset m1 ~ xl4 ~ offset xl2)
            case cs of
                c':_ -> standingCard (s + 1) c'
                    ! classList (column s6 ~ m5 ~ xl4)
                _    -> empty
        standingCards 2 (s + 2) $ drop 1 cs
standingCards 2 s cs = do
    standingCards 3 s $ take 4 cs
    standingCards 3 (s + 4) $ drop 4 cs

standingCards _ s cs = do
    row $ forWithIndex_ cs \i c ->
        standingCard (s + i) c ! classList (column s6 ~ m4 ~ xl3)

prominentStandingCard :: Int -> Comparable -> HTML Event
prominentStandingCard s c = div do
    monologue $ suffixOrdinal s
    comparableCard c

standingCard :: Int -> Comparable -> HTML Event
standingCard s c = div do
    sectionHeader $ suffixOrdinal s
    comparableCard c

comparableCard :: Comparable -> HTML Event
comparableCard c = div ! className (card ~ hoverable) $ do
    comparableCardMain c
    comparableCardBase c

suffixOrdinal :: Int -> String
suffixOrdinal i = show i <> case unit of
    _ | j == 1 && k /= 11 -> "st"
    _ | j == 2 && k /= 12 -> "nd"
    _ | j == 3 && k /= 13 -> "rd"
    _                     -> "th"
  where
    j = mod i 10
    k = mod i 100
