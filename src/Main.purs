module Main where

import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Prelude
import Pux (start)
import Pux.Renderer.React (renderToDOM)
import Signal ((~>))
import Signal.Channel (channel, send, subscribe)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget) as D
import Web.HTML.Location (hash)
import Web.HTML.Window (location, toEventTarget, document)
import Web.UIEvent.KeyboardEvent (fromEvent, key)

import Yousort.Component.State (Comparable(..), defaultState, State)
import Yousort.Component.Update (Event(..), foldp)
import Yousort.Component.View (view)
import Yousort.Route (fromHash)


main :: Effect Unit
main = do
    w <- window
    hashSignal <- sampleHash w
    let routeSignal = hashSignal ~> fromHash >>> View

    d <- document w
    keydownSignal <- sampleKeydown d
    let keydownSignal' = keydownSignal ~> KeyDown

    app <- start { initialState
                 , view
                 , foldp
                 , inputs: [routeSignal, keydownSignal']
                 }

    renderToDOM "#app" app.markup app.input
  where
    sampleHash w = do
        l <- location w
        c <- hash l >>= channel
        el <- eventListener \_ -> hash l >>= send c
        addEventListener (EventType "popstate") el false $ toEventTarget w

        pure $ subscribe c

    sampleKeydown d = do
        c <- channel "_Init"
        el <- eventListener \e -> do
            case fromEvent e of
                Just e' -> send c $ key e'
                _       -> pure unit

        addEventListener (EventType "keydown") el false $ D.toEventTarget d

        pure $ subscribe c

initialState :: State
initialState = defaultState
    { inputEntries = fromFoldable
        [ Image { caption: "Java"
                , url: "https://sdtimes.com/wp-content/uploads/2018/03/jW4dnFtA_400x400.jpg"
                , description: mempty
                }
        , Image { caption: "C"
                , url: "https://upload.wikimedia.org/wikipedia/commons/3/35/The_C_Programming_Language_logo.svg"
                , description: mempty
                }
        , Image { caption: "C++"
                , url: "https://upload.wikimedia.org/wikipedia/commons/1/18/ISO_C%2B%2B_Logo.svg"
                , description: mempty
                }
        , Image { caption: "Python"
                , url: "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c3/Python-logo-notext.svg/1024px-Python-logo-notext.svg.png"
                , description: mempty
                }
        , Text { title: "Visual Basic"
               , description: mempty
               }
        , Image { caption: "C#"
                , url: "https://pluralsight.imgix.net/paths/path-icons/csharp-e7b8fcd4ce.png"
                , description: mempty
                }
        , Image { caption: "PHP"
                , url: "https://upload.wikimedia.org/wikipedia/commons/2/27/PHP-logo.svg"
                , description: mempty
                }
        , Image { caption: "JavaScript"
                , url: "https://pluralsight.imgix.net/paths/path-icons/javascript-36f5949a45.png"
                , description: mempty
                }
        , Text { title: "SQL"
               , description: mempty
               }
        , Image { caption: "Swift"
                , url: "https://developer.apple.com/assets/elements/icons/swift/swift-64x64_2x.png"
                , description: mempty
                }
        , Image { caption: "MATLAB"
                , url: "https://upload.wikimedia.org/wikipedia/commons/2/21/Matlab_Logo.png"
                , description: mempty
                }
        , Image { caption: "Go"
                , url: "https://golang.org/doc/gopher/frontpage.png"
                , description: mempty
                }
        , Text { title: "Assembly language"
               , description: mempty
               }
        , Image { caption: "R"
                , url: "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/R_logo.svg/1448px-R_logo.svg.png"
                , description: mempty
                }
        , Image { caption: "Objective-C"
                , url: "https://woboq.com/blog/objective-c-for-qt-developers-meme.jpg"
                , description: mempty
                }
        , Text { title: "Perl"
               , description: mempty
               }
        , Image { caption: "Ruby"
                , url: "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Ruby_logo.svg/480px-Ruby_logo.svg.png"
                , description: mempty
                }
        , Image { caption: "Scala"
                , url: "https://www.scala-lang.org/resources/img/frontpage/scala-spiral.png"
                , description: mempty
                }
        , Image { caption: "F#"
                , url: "https://fsharp.org/img/logo/fsharp256.png"
                , description: mempty
                }
        , Image { caption: "Lua"
                , url: "https://www.lua.org/images/logo.gif"
                , description: mempty
                }
        , Image { caption: "Haskell"
                , url: "https://www.fpcomplete.com/hubfs/haskell_logo.svg"
                , description: mempty
                }
        , Image { caption: "PureScript"
                , url: "https://upload.wikimedia.org/wikipedia/commons/6/64/PureScript_Logo.png"
                , description: mempty
                }
        ]
    }
