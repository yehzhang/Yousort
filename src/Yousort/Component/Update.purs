module Yousort.Component.Update
    ( Event(..)
    , EditingEntryField(..)
    , foldp
    , viewCssClass
    , minimumPlayersCount
    , validateModification
    ) where

import Control.Alt ((<|>))
import Data.Array (fromFoldable) as A
import Data.Foldable (foldl, length, null)
import Data.List (List, deleteAt, head, takeWhile, updateAt, (!!), (:))
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (wrap)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds)
import Effect.Aff (delay) as A
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Materialize.DOM (destroy, findElements, findInstances, init)
import Materialize.Markup (liftVariadic, class Variadic)
import Materialize.Media.Images (MaterialBox)
import Materialize.Toasts (dismissAllToasts, toastS)
import Materialize.Tooltips (Tooltip)
import Materialize.Types (Class)
import Prelude
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent, targetValue)
import Web.DOM.Element (toParentNode) as E
import Web.DOM.ParentNode (ParentNode, querySelector)
import Web.Event.Event (preventDefault)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLDocument (toParentNode) as HD
import Web.HTML.HTMLElement (toParentNode) as HE
import Web.HTML.History (DocumentTitle(..), URL(..), pushState)
import Web.HTML.Window (document, history)

import Data.Tournament (Tournament, areExhaustive, fromFoldable, hold, play, unhold, unplay)
import Effect.Random.Shuffle (shuffle)
import Yousort.Component.State (Comparable(..), Operation(..), State, newText, toNewEntry, validateComparable)
import Yousort.Route (Route(..), fromHash, toHash)


data Event
    = KeyDown String

    -- High-level events from Opening.
    | InitEditing
    | CommitEditing
    | AbortEditing
    | DeleteChipAt Int

    | SetEditingEntry Comparable
    | SetEditingEntryField EditingEntryField DOMEvent
    | ClearEditingEntry

    | SetTargetEntryIndex Int
    | ClearTargetEntryIndex

    | AddEntry Comparable
    | UpdateEntryAt Int Comparable
    | DeleteEntryAt Int

    | NewTournament
    | StartTournament (Array Comparable)
    | Forward Operation
    | Backward

    -- | Sets the view to render.
    | View Route

    -- | Updates `document.location` and sets the view to render.
    | Navigate String DOMEvent
    | Navigate' Route

    | AtomicallyUpdateState (Array Event)

    | InitUncontrolledCompoenntsOnViewChanged
    | InitUncontrolledComponentsOnEvent

    | Noop

data EditingEntryField = Title
                       | Description
                       | Url
                       | Caption

foldp :: Event -> State -> EffModel State Event
foldp (KeyDown k) s@{ route: r, editingEntry: c } = onlyEffects s
    [ liftEvent case r, k of
        Opening, "Enter"  -> case isJust c of
            true -> CommitEditing
            _    -> InitEditing
        Opening, "Escape" -> AbortEditing
        _, _              -> Noop
    ]

foldp InitEditing s@{ inputEntries: cs } = onlyEffects s
    [ liftEvent $ SetEditingEntry $ maybe newText toNewEntry $ head cs
    ]

foldp CommitEditing s@{ inputEntries: cs
                      , editingEntry: ec
                      , targetEntryIndex: ti } = onlyEffects s
    [ liftEvent $ AtomicallyUpdateState case ti, ec of
        Just ti', Just c -> case validateModification ti' cs c of
            true -> [ ClearEditingEntry
                    , ClearTargetEntryIndex
                    , UpdateEntryAt ti' c
                    ]
            _    -> []
        _, Just c        -> case validateComparable c of
            true -> [ ClearEditingEntry
                    , AddEntry c
                    ]
            _    -> []
        _, _             -> []
    ]

foldp AbortEditing s = onlyEffects s
    [ liftEvent $ AtomicallyUpdateState [ ClearEditingEntry
                                        , ClearTargetEntryIndex
                                        ]
    ]

foldp (DeleteChipAt i) s@{ targetEntryIndex: ti } = onlyEffects s
    [ liftEvent $ AtomicallyUpdateState case maybe false (i == _) ti of
        true -> [ ClearEditingEntry
                , ClearTargetEntryIndex
                , DeleteEntryAt i
                ]
        _    -> [ DeleteEntryAt i
                ]
    ]

foldp (SetEditingEntry c) s = noEffects s { editingEntry = pure c }

foldp (SetEditingEntryField i e) s@{ editingEntry: c } = noEffects
    s { editingEntry = c <#> case _, i of
            Text t, Title                -> Text t { title = v' }
            Text t, Description          -> Text t { description = v' }
            Image i', Url                -> Image i' { url = v' }
            Image i', Caption            -> Image i' { caption = v' }
            Image i', Description        -> Image i' { description = v' }
            Audio a, Url                 -> Audio a { url = v' }
            Audio a, Caption             -> Audio a { caption = v' }
            Audio a, Description         -> Audio a { description = v' }
            YouTubeVideo v, Url          -> YouTubeVideo v { url = v' }
            YouTubeVideo v, Caption      -> YouTubeVideo v { caption = v' }
            YouTubeVideo v, Description  -> YouTubeVideo v { description = v' }
            c', _                        -> c'
      }
  where
    v' = targetValue e

foldp ClearEditingEntry s = noEffects s { editingEntry = Nothing }

foldp (SetTargetEntryIndex i) s = noEffects s { targetEntryIndex = pure i }

foldp ClearTargetEntryIndex s = noEffects s { targetEntryIndex = Nothing }

foldp (AddEntry c) s@{ inputEntries: cs, targetEntryIndex: ti }
    | validateComparable c = noEffects s { inputEntries = c:cs
                                         , targetEntryIndex = (_ + 1) <$> ti
                                         }
    | otherwise       = noEffects s

foldp (UpdateEntryAt i c) s@{ inputEntries: cs }
    | validateComparable c = noEffects $
        fromMaybe s $ s { inputEntries = _ } <$> updateAt i c cs
    | otherwise            = noEffects s

foldp (DeleteEntryAt i) s@{ inputEntries: cs, targetEntryIndex: ti } = noEffects
    $ deleteAt i cs
    <#> s { inputEntries = _
          , targetEntryIndex = ti >>= \ti' ->
                case compare ti' i of
                    LT -> pure ti'
                    EQ -> Nothing
                    GT -> pure $ ti' - 1
          }
    # fromMaybe s

foldp NewTournament s@{ inputEntries: cs } = onlyEffects s
    [ liftEffect $ pure <<< StartTournament <$> shuffle (A.fromFoldable cs)
    ]

foldp (StartTournament cs) s =
    { state: s { history = mempty
               , tournament = fromFoldable cs
               } :: State
    , effects: [ liftEvent $ Navigate' Tournament
               ] }

foldp (Forward Hold) s@{ history: os, tournament: t } =
    case areExhaustive hs t of
        true -> onlyEffects s
            [ toastS "It's time to make a choice." $> Nothing ]
        _    ->
            { state: s { history = Hold : os
                       , tournament = hold hs t
                       }
            , effects: [ liftEvent InitUncontrolledComponentsOnEvent
                       ] }
  where
    hs = countHolds os
foldp (Forward o@(Choose r)) s@{ history: os, tournament: t } =
    { state: play r t
        <#> (\t' -> s { history = o:os
                      , tournament = t'
                      })
        # fromMaybe s
    , effects: [ liftEvent InitUncontrolledComponentsOnEvent
               ] }

foldp Backward s@{ history: o:os } =
    { state: backward o s
        <#> (\t -> s { history = os
                     , tournament = t
                     })
        # fromMaybe s
    , effects: [ liftEvent InitUncontrolledComponentsOnEvent
               ] }
foldp Backward s                   = onlyEffects s $
    [ liftEvent $ Navigate' Opening ]

foldp (View r) s@{ tournament: t }
    | r == Tournament && null t = onlyEffects s [ liftEvent $ Navigate' Opening ]
    | r == s.route              = noEffects s
    | otherwise                 =
        { state: s { route = r
                   , canResume = not $ null t }
        , effects: [ liftEffect do
                        dismissAllToasts
                        _ :: Array Tooltip <-
                            viewNode >>= findInstances >>= traverse destroy

                        liftEvent InitUncontrolledCompoenntsOnViewChanged
                   ] }

foldp (Navigate u e) s = onlyEffects s
    [ liftEffect $ preventDefault e *> route (fromHash u) u ]

foldp (Navigate' r) s = onlyEffects s [ liftEffect $ route r $ toHash r ]

foldp (AtomicallyUpdateState es) s = foldl foldModel (noEffects s) es
  where
    foldModel { state: s', effects: es' } e
        | m@{ effects: es'' } <- foldp e s' = m { effects = es'' <> es' }

foldp InitUncontrolledCompoenntsOnViewChanged s = onlyEffects s
    [ liftEffect do
        _ :: Array Tooltip <- viewNode >>= findElements
            >>= traverse (init { enterDelay: 400.0
                               , exitDelay: 50.0
                               , inDuration: 40.0
                               , outDuration: 40.0
                               , exitOnClick: true
                               })
        liftEvent InitUncontrolledComponentsOnEvent
    ]

foldp InitUncontrolledComponentsOnEvent s = onlyEffects s
    [ liftEffect do
        _ :: Array MaterialBox <- viewNode >>= findElements
            >>= traverse (init {})
        pure Nothing
    ]

foldp Noop s = noEffects s

route :: Route -> String -> Effect (Maybe Event)
route r u = do
    window
        >>= history
        >>= pushState (unsafeToForeign {}) (DocumentTitle "") (URL u)
    liftEvent $ View r

viewNode :: Effect ParentNode
viewNode = do
    d <- window >>= document
    let pd = HD.toParentNode d
    v <- querySelector (wrap viewCssClass) pd

    b <- body d
    pure $ fromMaybe pd $ E.toParentNode <$> v <|> HE.toParentNode <$> b

backward :: Operation -> State -> Maybe (Tournament Comparable)
backward (Choose r) { tournament: t }       = unplay r t
backward Hold { history: h, tournament: t } = Just $ unhold (countHolds h) t

countHolds :: forall a. Semiring a => List Operation -> a
countHolds = length <<< takeWhile (eq Hold)

minimumPlayersCount :: Int
minimumPlayersCount = 2

delay :: forall a. Milliseconds -> a -> Aff (Maybe a)
delay t a = A.delay t $> Just a

liftEvent :: forall m. Monad m => Event -> m (Maybe Event)
liftEvent = pure <<< Just

viewCssClass :: forall r. Variadic Class r => r
viewCssClass = liftVariadic "app-view"

validateModification :: Int -> List Comparable -> Comparable -> Boolean
validateModification ti' cs c =
    maybe false (c /= _) (cs !! ti') && validateComparable c
