module Yousort.Route
    ( Route(..)
    , fromPathname
    , toPathname
    , fromHash
    , toHash
    ) where

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (uncons)
import Prelude
import Pux.Router (end, router, lit)


data Route = NotFound String
           | Opening
           | Tournament
           | Award

fromPathname :: String -> Route
fromPathname u = orDefault u $ fromPathname' u

fromPathname' :: String -> Maybe Route
fromPathname' u  = router u $
        Opening <$ end
    <|> Tournament <$ lit "tournament" <* end
    <|> Award <$ lit "award" <* end

toPathname :: Route -> String
toPathname (NotFound u) = u
toPathname Opening      = "/"
toPathname Tournament   = "/tournament"
toPathname Award        = "/award"

fromHash :: String -> Route
fromHash "" = Opening
fromHash u  = orDefault u do
    { head: h, tail: t } <- uncons u
    case h of
        '#' -> fromPathname' t
        _   -> Nothing

toHash :: Route -> String
toHash u = "#" <> toPathname u

orDefault :: String -> Maybe Route -> Route
orDefault = fromMaybe <<< NotFound

derive instance eqRoute :: Eq Route
