module Yousort.Component.View.Opening
    ( opening
    ) where

import Data.Foldable (length)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (null) as S
import GitHub.Corners (defaultCorner)
import Materialize.Alignment (align)
import Materialize.Cards (cardTitle, cardContent, card)
import Materialize.Forms.Chips (chip)
import Materialize.Forms.TextInputs (dataValidationError, helperText, inputField, invalid)
import Materialize.Grid (column, m1, m10, m3, offset, s11, s12, s6, xl2, xl8, container)
import Materialize.Icons (icon, prefixedInInputField)
import Materialize.Icons.Data (Icons, add, image, link, shortText, subtitles, audiotrack, playArrow)
import Materialize.Icons.Data (close, title) as I
import Materialize.Markup (classList, liftVariadic, (~))
import Materialize.Overriden (active, right)
import Materialize.Text (text) as M
import Materialize.Waves (waves)
import Prelude hiding (div, add)
import Pux.DOM.Events (onChange, onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, input, label, span)
import Text.Smolder.HTML.Attributes (className, for, id, target, type', value)
import Text.Smolder.Markup (empty, text, (!), (#!))

import Yousort.Component.Internal ((=^=))
import Yousort.Component.State (Comparable(..), Media, State, newAudio, newImage, newText, newYouTubeVideo, parseYouTubeVideoId, validateComparable)
import Yousort.Component.Update (Event(..), minimumPlayersCount, EditingEntryField(..), validateModification)
import Yousort.Component.View.Common (anchor, autofocus, cardActions, centered, checked, chipPrefix, chipSuffix, clickable, description, disabled, divider, iconButton', interruptiveColor, key, onClickCancel, onClickNotCanceled, primaryColor, primaryTextColor, row, section, sectionHeader, sectionRow, textButton, textEmphasis, title)
import Yousort.Route (Route(..))


opening :: State -> HTML Event
opening { inputEntries: cs
        , canResume: rb
        , editingEntry: ec
        , targetEntryIndex: ti
        } = div do
    defaultCorner "https://github.com/yehzhang/Yousort" ! target "_blank"

    let nEntriesLeft = minimumPlayersCount - length cs
        canStart = nEntriesLeft <= 0
    section do
        title "Yousort"

        description "Add any kind of entries, compare them, and see them ordered automatically."

        let startTournament = case canStart of
                true -> textButton case rb of
                        true -> "Start new run"
                        _    -> "Start comparing"
                    #! onClick (const NewTournament)
                _    -> textButton (show nEntriesLeft <> " entries left")
                    ! disabled true
        sectionRow case rb of
            true -> do
                div ! classList do
                        column s6
                        align right
                    $ startTournament
                div ! classList (column s6)
                    $ textButton "Resume last run"
                        #! onClick (const $ Navigate' Tournament)
            _    -> centered startTournament

    section do
        sectionHeader "Entries"

        sectionRow do
            centered do
                iconButton' "Add an entry" add case canStart of
                        true -> primaryColor
                        _    -> interruptiveColor
                    ! disabled (isJust ec)
                    #! onClick (const InitEditing)

            maybe empty (sectionRow <<< editingEntry) ec

            -- Chips
            div ! className container $ sectionRow $
                forWithIndex_ cs \i' c -> let selected = pure i' == ti in a
                    ! classList do
                        chip
                        clickable
                        waves ~ primaryColor
                        when selected do
                            liftVariadic primaryColor
                            M.text primaryTextColor
                    ! key c
                    -- Focus the chip
                    #! onClickNotCanceled (const $
                        AtomicallyUpdateState [ SetTargetEntryIndex i'
                                              , SetEditingEntry c
                                              ])
                    $ do
                        case c of
                            Text { title: t }            -> do
                                chipPrefix shortText
                                text t
                            Image { caption: c' }        -> do
                                chipPrefix image
                                text c'
                            Audio { caption: c' }        -> do
                                chipPrefix audiotrack
                                text c'
                            YouTubeVideo { caption: c' } -> do
                                chipPrefix playArrow
                                text c'
                        span #! onClickCancel (const $ DeleteChipAt i')
                            $ chipSuffix I.close
  where
    editingEntry :: Comparable -> HTML Event
    editingEntry c = div
        ! classList (column s12 ~ m10 ~ offset m1 ~ xl8 ~ offset xl2)
        $ div ! className card $ do
            div ! className cardContent $ do
                div do
                    div ! className cardTitle $ text "Type"
                    sectionRow do
                        radio "Text" c newText
                        radio "Image" c newImage
                        radio "Audio" c newAudio
                        radio "YouTube" c newYouTubeVideo
                divider
                row $ inputFields c
            cardActions do
                anchor #! onClick (const AbortEditing) $ text "Cancel"
                case ti of
                    Just ti' -> anchor
                        ! disabled (not $ validateModification ti' cs c)
                        $ emphasize "Save"
                    _        -> anchor
                        ! disabled (not $ validateComparable c)
                        $ emphasize "Add"
                    #! onClick (const CommitEditing)

inputFields :: Comparable -> HTML Event
inputFields (Text { title: t, description: d }) = do
    textInput' "Title" I.title Title t true Nothing
    textInput "Description (optional)" subtitles Description d
inputFields (Image m) = mediaInputFields m
inputFields (Audio m) = mediaInputFields m
inputFields (YouTubeVideo { url: u, caption: c', description: d }) = do
    textInput' "Caption" subtitles Caption c' true Nothing
    textInput' "URL" link Url u false $ case S.null u of
        true -> Nothing
        _    -> maybe (pure "Invalid YouTube URL") (const Nothing) $
            parseYouTubeVideoId u
    textInput "Description (optional)" subtitles Description d

mediaInputFields :: Media -> HTML Event
mediaInputFields { url: u, caption: c', description: d } = do
    textInput' "Caption" subtitles Caption c' true Nothing
    textInput "URL" link Url u
    textInput "Description (optional)" subtitles Description d

emphasize :: String -> HTML Event
emphasize t = span ! classList textEmphasis $ text t

radio :: String -> Comparable -> Comparable -> HTML Event
radio l c newC = div ! classList (column s6 ~ m3) $
    label do
        input ! type' "radio"
            ! checked (c =^= newC)
            #! onChange (const $ SetEditingEntry newC)
        span $ text l

textInput :: Label
          -> Icons
          -> EditingEntryField
          -> DefaultValue
          -> HTML Event
textInput l i f v = textInput' l i f v false Nothing

textInput' :: Label
           -> Icons
           -> EditingEntryField
           -> DefaultValue
           -> AutoFocus
           -> Maybe InvalidHelperText
           -> HTML Event
textInput' l i f v a e = div ! classList do
        column s11
        inputField
    $ do
        let inputFieldId = "_generated_" <> l
        icon i ~ prefixedInInputField
        input ! type' "text"
            ! autofocus a
            ! classList do
                when (isJust e) invalid
            ! id inputFieldId
            ! value v
            #! onChange (SetEditingEntryField f)
        label ! for inputFieldId
            ! classList do
                when (not $ S.null v) $ liftVariadic active
            $ text l
        maybe empty (\e' ->
            span ! className helperText ! dataValidationError e' $ empty) e

type Label = String
type DefaultValue = String
type InvalidHelperText = String
type AutoFocus = Boolean
