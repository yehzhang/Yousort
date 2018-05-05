module Yousort.Component.View.Common
    ( navigation
    , title
    , monologue
    , description
    , sectionHeader
    , sectionRow
    , section
    , row
    , centered
    , anchor
    , textButton
    , textButton'
    , iconButton
    , iconButton'
    , cardActions
    , comparableCardMain
    , chipPrefix
    , chipSuffix
    , divider
    , autofocus
    , primaryColor
    , nonInterruptiveColor
    , interruptiveColor
    , primaryTextColor
    , disabled
    , checked
    , controls
    , allowfullscreen
    , clickable
    , textEmphasis
    , chipPrefixIcon
    , chipSuffixIcon
    , key
    , onClickNotCanceled
    , onClickCancel
    , comparableCardBase
    ) where

import CSS (borderRadius, maxHeight, nil, pct, px, vh, width)
import CSS.Overflow (overflow, scroll)
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.Typelevel.Num (d1, d2)
import Materialize.Markup (class Variadic, classList, liftVariadic, renderString, (~))
import Materialize.Types (Class)
import Materialize.Grid (divider, row, section) as M
import Materialize.Alignment (align, horizontalCenter, verticalCenter)
import Materialize.Buttons (button, floating)
import Materialize.Cards (cardAction, cardImage, cardContent, cardTitle)
import Materialize.Color (Color, background, blueGrey, lighten, red, white)
import Materialize.Grid (column, s1)
import Materialize.Media.Images (image, materialBoxed)
import Materialize.Media.Videos (video)
import Materialize.Icons (icon, materialIcons)
import Materialize.Icons.Data (Icons, home)
import Materialize.Overriden (responsive, right)
import Materialize.Waves (light, waves)
import Materialize.Tooltips (dataTooltip, tooltipped)
import Prelude hiding (div, add)
import Pux.DOM.Events (onClick, DOMEvent)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (style)
import Pux.DOM.HTML.Attributes (key) as P
import Text.Smolder.HTML (a, audio, div, h1, h3, h4, i, iframe, img, p)
import Text.Smolder.HTML.Attributes (src, className, alt)
import Text.Smolder.HTML.Attributes (autofocus, checked, controls, disabled) as A
import Text.Smolder.Markup (Attribute, EventHandlers, empty, text, (!), (#!), attribute)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (defaultPrevented, preventDefault)

import Yousort.Component.Update (Event(..))
import Yousort.Component.State (Comparable(..), Media, parseYouTubeVideoId)
import Yousort.Route (Route(..))


navigation :: HTML Event
navigation = sectionRow $ div ! classList do
        column s1
        align horizontalCenter
    $ iconButton' "Return to home" home nonInterruptiveColor
        #! onClick (const $ Navigate' Opening)

title :: String -> HTML Event
title t = h1 ! classList (align horizontalCenter) $ text t

monologue :: String -> HTML Event
monologue m = h3 ! classList (align horizontalCenter) $ text m

description :: String -> HTML Event
description p' = p ! classList (align horizontalCenter) $ text p'

sectionHeader :: String -> HTML Event
sectionHeader h = h4 ! classList (align horizontalCenter) $ text h

section :: HTML Event -> HTML Event
section = div ! classList M.section

sectionRow :: HTML Event -> HTML Event
sectionRow = div ! classList do
    M.row
    M.section

row :: HTML Event -> HTML Event
row = div ! classList M.row

centered :: HTML Event -> HTML Event
centered = div ! classList (align horizontalCenter)

anchor :: HTML Event -> HTML Event
anchor = a ! classList clickable

textButton :: String -> HTML Event
textButton = flip textButton' primaryColor

textButton' :: String -> Color -> HTML Event
textButton' t b = a ! classList do
        button
        waves ~ light
        liftVariadic b
    $ text t

iconButton :: String -> Icons -> HTML Event
iconButton t i = iconButton' t i primaryColor

iconButton' :: String -> Icons -> Color -> HTML Event
iconButton' t i b = a ! classList do
        button ~ floating
        waves ~ light
        liftVariadic b
        tooltipped
    ! dataTooltip t
    $ icon i

cardActions :: HTML Event -> HTML Event
cardActions = div ! classList do
    cardAction
    align right

comparableCardMain :: Comparable -> HTML Event
comparableCardMain (Text _) = empty
comparableCardMain (Image { url: u, caption: c }) = div
    ! classList do
        cardImage
        align verticalCenter
    ! style (maxHeight $ vh 30.0)
    $ div
        ! style (width $ pct 100.0)
        $ img ! className (image materialBoxed) ! src u ! alt c
comparableCardMain (Audio { url: u }) = div ! classList do
        M.section
        align horizontalCenter
    ! style (overflow scroll)
    $ audio ! src u ! controls true $ empty
comparableCardMain (YouTubeVideo { url: u }) = div ! className cardImage $
    div ! className (video responsive true)
        ! style (borderRadius (px 2.0) (px 2.0) nil nil)
        $ case parseYouTubeVideoId u of
            Just i -> iframe
                ! src ("https://www.youtube.com/embed/" <> i <> "?rel=0")
                ! attribute "frameborder" "0"
                ! attribute "allow" "autoplay; encrypted-media"
                ! allowfullscreen true
                $ empty
            _      -> empty

comparableCardBase :: Comparable -> HTML Event
comparableCardBase (Text { title: t, description: d }) = div
    ! className cardContent
    $ case null d of
        true -> div ! classList do
                cardTitle
                align horizontalCenter
            $ text t
        _    -> do
            div ! className cardTitle $ text t
            p $ text d
comparableCardBase (Image m) = mediaInfoCardContent m
comparableCardBase (Audio m) = mediaInfoCardContent m
comparableCardBase (YouTubeVideo m) = mediaInfoCardContent m

mediaInfoCardContent :: Media -> HTML Event
mediaInfoCardContent { caption: c, description: d } = div
    ! className cardContent
    $ do
        div ! className cardTitle $ text c
        case null d of
            true -> empty
            _    -> p $ text d

chipPrefix :: Icons -> HTML Event
chipPrefix i' = i ! classList do
        materialIcons
        chipPrefixIcon
    $ text $ renderString i'

chipSuffix :: Icons -> HTML Event
chipSuffix i' = i ! classList do
        materialIcons
        chipSuffixIcon
    $ text $ renderString i'

divider :: HTML Event
divider = section $
    div ! classList M.divider $ empty

primaryColor :: Color
primaryColor = background blueGrey ~ lighten d1

nonInterruptiveColor :: Color
nonInterruptiveColor = background blueGrey ~ lighten d2

interruptiveColor :: Color
interruptiveColor = background red

primaryTextColor :: Color
primaryTextColor = background white

disabled :: Boolean -> Attribute
disabled = toBooleanAttribute A.disabled

checked :: Boolean -> Attribute
checked = toBooleanAttribute A.checked

controls :: Boolean -> Attribute
controls = toBooleanAttribute A.controls

allowfullscreen :: Boolean -> Attribute
allowfullscreen = booleanAttribute "allowfullscreen"

toBooleanAttribute :: (String -> Attribute) -> Boolean -> Attribute
toBooleanAttribute = booleanAttribute' "__DummyValue__"

booleanAttribute :: String -> Boolean -> Attribute
booleanAttribute a = booleanAttribute' a $ attribute a

booleanAttribute' :: String -> (String -> Attribute) -> Boolean -> Attribute
booleanAttribute' t f b = f case b of
    true -> t
    _    -> ""  -- which results in this attribute getting stripped by React.

autofocus :: Boolean -> Attribute
autofocus = toBooleanAttribute A.autofocus

clickable :: forall r. Variadic Class r => r
clickable = liftVariadic "clickable"

textEmphasis :: forall r. Variadic Class r => r
textEmphasis = liftVariadic "text-emphasis"

chipPrefixIcon :: forall r. Variadic Class r => r
chipPrefixIcon = liftVariadic "chip-prefix-icon"

chipSuffixIcon :: forall r. Variadic Class r => r
chipSuffixIcon = liftVariadic "chip-suffix-icon"

key :: forall a. Show a => a -> Attribute
key = P.key <<< show

onClickNotCanceled :: (DOMEvent -> Event) -> EventHandlers (DOMEvent -> Event)
onClickNotCanceled f = onClick \e -> case defaultPrevented e of
    true -> Noop
    _    -> f e

onClickCancel :: (DOMEvent -> Event) -> EventHandlers (DOMEvent -> Event)
onClickCancel f = onClick \e ->
    -- A hack to mark this event as canceled.
    unsafeRunEffect (preventDefault e) $ f e
  where
    unsafeRunEffect e' = flip const $ unsafeCoerce e' unit
