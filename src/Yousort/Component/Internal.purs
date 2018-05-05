module Yousort.Component.Internal where

import Data.Generic.Rep (class Generic, Constructor, Sum(..), from)
import Prelude


class GenericCtorEq a where
    genericCtorEq' :: a -> a -> Boolean

instance genericEqSum :: (GenericCtorEq a, GenericCtorEq b) => GenericCtorEq (Sum a b) where
    genericCtorEq' (Inl a) (Inl a') = genericCtorEq' a a'
    genericCtorEq' (Inr b) (Inr b') = genericCtorEq' b b'
    genericCtorEq' _ _              = false

instance genericEqConstructor :: GenericCtorEq (Constructor a b) where
    genericCtorEq' _ _ = true

genericCtorEq :: forall a b. Generic a b => GenericCtorEq b => a -> a -> Boolean
genericCtorEq x = genericCtorEq' (from x) <<< from

infixl 4 genericCtorEq as =^=
