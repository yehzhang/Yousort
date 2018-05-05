module Yousort.Component.View where

import CSS (maxWidth, pct, width, backgroundColor, border, borderBottom, boxShadow, color, float, floatLeft, floatRight, fontSize, fontWeight, fromInt, fromString, key, lineHeight, marginBottom, marginLeft, marginRight, nil, paddingLeft, paddingRight, px, solid, weight, (?))
import CSS.Overflow (overflow, hidden)
import Materialize.Grid (container)
import Materialize.Markup (classList, liftVariadic, (~))
import Materialize.Overriden (large, medium, small)
import Materialize.Visibility (hide, on)
import Prelude hiding (div)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.Markup (text, (!))

import Yousort.Component.State (State)
import Yousort.Component.Update (Event, viewCssClass)
import Yousort.Component.View.Award (award)
import Yousort.Component.View.Common (section)
import Yousort.Component.View.NotFound (notFound)
import Yousort.Component.View.Opening (opening)
import Yousort.Component.View.Tournament (tournament)
import Yousort.Route (Route(..))


view :: State -> HTML Event
view s@{ route: r } = do
    -- Debug info
    when false $ section do
        div ! classList (hide ~ on medium ~ on large) $ text "Small"
        div ! classList (hide ~ on small ~ on large) $ text "Medium"
        div ! classList do
                hide ~ on small ~ on medium
                liftVariadic "hide-on-extra-large-only"
            $ text "Large"

    style do
        let cursor = key $ fromString "cursor"
        fromString ".clickable" ? cursor "pointer"

        fromString ".row" ? marginBottom nil

        fromString ".app-view" ? do
            maxWidth $ px 1080.0

        -- Radio buttons
        let primaryColor = fromInt 0x78909c
        let primaryColorDarkVariant = fromInt 0x607d8b  -- blue-grey
        let primaryColorLightVariant = fromInt 0xb0bec5  -- blue-grey lighten3
        let redColor = fromInt 0xf44336
        fromString """input[type="radio"]:checked + span:after,
                      input[type="radio"].with-gap:checked + span:before,
                      input[type="radio"].with-gap:checked + span:after
                      """ ? do
            backgroundColor primaryColorDarkVariant
            border solid (px 2.0) primaryColorDarkVariant

        -- Text inputs
        fromString """.input-field input[type="text"]:not(.browser-default):focus:not([readonly]) + label,
                      .input-field .prefix.active""" ? do
            color primaryColorDarkVariant
        fromString """.input-field input[type="text"]:not(.browser-default):focus:not([readonly])""" ? do
            borderBottom solid (px 1.0) primaryColorDarkVariant
            boxShadow nil (px 1.0) nil primaryColorDarkVariant
        fromString """.input-field input[type=text].invalid:not(.browser-default),
                      .input-field input[type=text].invalid:not(.browser-default):focus""" ? do
            borderBottom solid (px 1.0) redColor
            boxShadow nil (px 1.0) nil redColor

        -- Card action
        fromString ".card .card-action a:not(.btn):not(.btn-large):not(.btn-small):not(.btn-large):not(.btn-floating)" ? do
            marginRight nil
            marginLeft (px 24.0)
            color primaryColorDarkVariant
        fromString ".card .card-action a:not(.btn):not(.btn-large):not(.btn-small):not(.btn-large):not(.btn-floating):hover" ? do
            color primaryColorLightVariant

        let pointerEvents = key $ fromString "pointer-events"
        fromString """.card-action a[disabled]:not(.btn):not(.btn-large):not(.btn-small):not(.btn-large):not(.btn-floating),
                      .card-action a[disabled]:not(.btn):not(.btn-large):not(.btn-small):not(.btn-large):not(.btn-floating):hover""" ? do
            pointerEvents "none"
            color $ fromInt 0xDFDFDF
            cursor "default"

        -- Font
        fromString ".text-emphasis" ? do
            fontWeight $ weight 600.0

        -- Chips
        let interruptiveColor = fromInt 0xF44336
        fromString ".chip-prefix-icon" ? do
            float floatLeft
            fontSize $ px 18.0
            lineHeight $ px 32.0
            paddingRight $ px 2.0
        fromString ".chip-suffix-icon" ? do
            float floatRight
            fontSize $ px 16.0
            lineHeight $ px 32.0
            paddingLeft $ px 8.0
        fromString ".chip-suffix-icon:hover" ? do
            color interruptiveColor
            fontWeight $ weight 600.0

        -- Waves
        fromString ".waves-effect.waves-blue-grey.waves-lighten-1 .waves-ripple" ? do
            backgroundColor primaryColor

        -- Material Box
        fromString ".material-placeholder" ? do
            width $ pct 100.0
        -- Cannot do this in element style as Material box removes it.
        fromString ".card-image.valign-wrapper" ? do
            overflow hidden

    div ! classList do
            viewCssClass
            container
        $ case r of
            Opening    -> opening s
            Tournament -> tournament s
            Award      -> award s
            NotFound _ -> notFound
