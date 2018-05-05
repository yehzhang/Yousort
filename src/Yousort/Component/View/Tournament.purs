module Yousort.Component.View.Tournament
    ( tournament
    ) where

import CSS (alignSelf, display, flex, flexEnd, marginLeft, minHeight, nil, vh)
import Data.Foldable (null)
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))
import Materialize.Alignment (align, horizontalCenter)
import Materialize.Cards (card, hoverable)
import Materialize.Color (black)
import Materialize.Grid (column, l2, l4, m3, offset, s1, s5, s6, xl1, xl5)
import Materialize.Icons.Data (lowPriority, undo)
import Materialize.Markup (classList, (~))
import Materialize.Text (text) as M
import Materialize.Waves (displayBlock, waves)
import Prelude hiding (div, add)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (a, div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!), (#!))

import Data.Tournament (GameResult(..), next)
import Yousort.Component.State (Comparable, Operation(..), State)
import Yousort.Component.Update (Event(..))
import Yousort.Component.View.Common (centered, clickable, comparableCardBase, comparableCardMain, description, disabled, iconButton, key, navigation, section, sectionHeader, sectionRow, textButton)
import Yousort.Route (Route(..))


tournament :: State -> HTML Event
tournament { tournament: t, history: os } = do
    navigation

    let nextPlayers = next t
    section case nextPlayers of
        Just (Tuple c c') -> do
            description "Which goes first?"
            sectionRow ! style do
                    display flex
                    minHeight $ vh 45.0
                $ do
                    competitor Left c
                        ! classList (column s6 ~ xl5 ~ offset xl1)
                        ! style (alignSelf flexEnd)
                    competitor Right c'
                        ! classList (column s6 ~ xl5)
                        ! style do
                            alignSelf flexEnd
                            marginLeft nil
        _                 -> do
            description "Comparison completed."
            section do
                sectionHeader "See results?"
                section $ centered $
                    textButton "Yes!" #! onClick (const $ Navigate' Award)

    sectionRow do
        div ! classList do
                column s5 ~ offset s1
                    ~ m3 ~ offset m3
                    ~ l2 ~ offset l4
                    ~ xl1 ~ offset xl5
                align horizontalCenter
            $ iconButton "Undo" undo
                ! disabled (null os)
                #! onClick (const Backward)
        div ! classList do
                column s5 ~ m3 ~ l2 ~ xl1
                align horizontalCenter
            $ iconButton "Ask me later" lowPriority
                ! disabled (isNothing nextPlayers)
                #! onClick (const $ Forward Hold)

competitor :: GameResult -> Comparable -> HTML Event
competitor o c = div do
    div ! className (card ~ hoverable) ! key c $ do
        comparableCardMain c
        a ! classList do
                M.text black
                clickable
                waves ~ displayBlock
            $ comparableCardBase c #! onClick (const $ Forward $ Choose o)
