module Effect.Random.Shuffle
    ( shuffle
    ) where

import Control.Monad.ST (ST, run)
import Data.Array (length, range)
import Data.Array.ST (STArray, peek, poke, withArray)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Prelude


shuffle :: forall a. Array a -> Effect (Array a)
shuffle xs = do
    ss <- generateShuffle $ length xs - 1
    pure $ run (withArray (flip traverse ss <<< swap) xs)

generateShuffle :: UpperBound -> Effect (Array SwapByIndices)
generateShuffle ub = traverse (\i -> Tuple i <$> randomInt i ub) $ range 0 ub

type SwapByIndices = Tuple Int Int
type UpperBound = Int

swap :: forall h a. STArray h a -> SwapByIndices -> ST h Unit
swap xs (Tuple i j) = do
    xi <- peek i xs
    xj <- peek j xs
    case xi, xj of
        Just xi', Just xj' -> do
            void $ poke j xi' xs
            void $ poke i xj' xs
        _, _               -> pure unit
