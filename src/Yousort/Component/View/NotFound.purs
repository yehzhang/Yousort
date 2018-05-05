module Yousort.Component.View.NotFound
    ( notFound
    ) where

import Prelude
import Pux.DOM.HTML (HTML)

import Yousort.Component.Update (Event)
import Yousort.Component.View.Common (description, navigation, section)


notFound :: HTML Event
notFound = do
    navigation
    section $ description "Nothing is here."
