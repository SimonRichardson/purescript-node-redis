module Database.Redis.Commands.Range where

import Data.Tuple (Tuple(..))
import Prelude (($))

newtype Range s = Range (Tuple s s)

range :: forall a. a -> a -> Range a
range x y = Range $ Tuple x y

toArray :: forall a. Range a -> Array a
toArray (Range (Tuple a b)) = [a, b]
