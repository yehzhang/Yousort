module Test.Main where

import Effect (Effect)
import Prelude
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Test.Tournament (tournamentSpecs)


main :: Effect Unit
main = run [consoleReporter] do
    tournamentSpecs
