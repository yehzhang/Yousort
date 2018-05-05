module Test.QuickCheck.Class where

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Prelude
import Test.QuickCheck (class Testable)
import Test.QuickCheck (quickCheck) as QC


quickCheck :: forall prop. Testable prop => prop -> Aff Unit
quickCheck = liftEffect <<< QC.quickCheck
