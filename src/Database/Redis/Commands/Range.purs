module Database.Redis.Commands.Range where

import Data.Tuple
import Data.Tuple.Nested

import Prelude (($))

newtype Range s = Range (Tuple2 s s)

range :: forall a. a -> a -> Range a
range x y = Range $ tuple2 x y

toArray :: forall a. Range a -> Array a
toArray (Range (Tuple a b)) = [a, b]
