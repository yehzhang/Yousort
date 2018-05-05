module Yousort.Component.State
    ( State
    , Operation(..)
    , Comparable(..)
    , Media
    , Url
    , newText
    , newImage
    , newAudio
    , newYouTubeVideo
    , newMedia
    , toNewEntry
    , defaultState
    , validateComparable
    , parseYouTubeVideoId
    ) where

import Control.Alt ((<|>))
import Data.Array (head)
import Data.Array.NonEmpty (tail)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..), isJust)
import Data.String (null)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Typelevel.Undefined (undefined)
import Prelude

import Data.Tournament (GameResult, Tournament, fromFoldable)
import Yousort.Route (Route(..))


type State = { route :: Route
             , history :: List Operation
             , tournament :: Tournament Comparable
             , inputEntries :: List Comparable
             , canResume :: Boolean
             , editingEntry :: Maybe Comparable
             , targetEntryIndex :: Maybe Int
             }

data Operation = Choose GameResult
               | Hold

derive instance eqOperation :: Eq Operation

data Comparable = Text { title :: String, description :: String }
                | Image Media
                | Audio Media
                | YouTubeVideo Media

type Media = { url :: Url, caption :: String, description :: String }

type Url = String

newText :: Comparable
newText = Text { title: mempty, description: mempty }

newImage :: Comparable
newImage = Image newMedia

newAudio :: Comparable
newAudio = Audio newMedia

newYouTubeVideo :: Comparable
newYouTubeVideo = YouTubeVideo newMedia

newMedia :: Media
newMedia = { url: mempty, caption: mempty, description: mempty }

toNewEntry :: Comparable -> Comparable
toNewEntry (Text _)  = newText
toNewEntry (Image _) = newImage
toNewEntry (Audio _) = newAudio
toNewEntry (YouTubeVideo _) = newYouTubeVideo

defaultState :: State
defaultState = { route: NotFound undefined
               , history: mempty
               , tournament: fromFoldable []
               , inputEntries: mempty
               , canResume: false
               , editingEntry: Nothing
               , targetEntryIndex: Nothing
               }

validateComparable :: Comparable -> Boolean
validateComparable c = c /= toNewEntry c && validate c
  where
    validate (Text { title: t }) = not $ null t
    validate (Image m) = validateMedia m
    validate (Audio m) = validateMedia m
    validate (YouTubeVideo m@{ url: u }) =
        validateMedia m && (isJust $ parseYouTubeVideoId u)

    validateMedia { caption: c', url: u } = not $ null c' || null u

parseYouTubeVideoId :: Url -> Maybe String
parseYouTubeVideoId u = parseWatchUrl u <|> parseEmbedUrl u <|> parseId u
  where
    parseWatchUrl = parseWithRegex
        """^https?:\/\/(?:www\.)?youtube\.com\/watch\?.*v=([\w-]{11})$"""
    parseEmbedUrl = parseWithRegex
        """^https?:\/\/(?:www\.)?youtube\.com\/embed\/([\w-]{11})$"""
    parseId = parseWithRegex """^([\w-]{11})$"""
    parseWithRegex p s = do
        r <- hush $ regex p noFlags
        a <- match r s
        join $ head $ tail a

derive instance genericComparable :: Generic Comparable _

derive instance eqComparable :: Eq Comparable

instance showComparable :: Show Comparable where
    show (Text { title: t, description: d }) =
        "Text{title:\"" <> t <> ",description:\"" <> d <> "\"}"
    show (Image m) = "Image" <> showMedia m
    show (Audio m) = "Audio" <> showMedia m
    show (YouTubeVideo m) = "YouTubeVideo" <> showMedia m

showMedia :: Media -> String
showMedia { url: u, caption: c, description: d } =
    "{url:\"" <> u
    <> ",caption:\"" <> c
    <> ",description:\"" <> d
    <> "\"}"
