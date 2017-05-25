module Database.Redis.Commands.Values where

import Data.Tuple

import Database.Redis.Commands.Field
import Database.Redis.Commands.Property

import Prelude

newtype Values a b = Values (Tuple (Field a) b)

instance valValues :: (Val b) => Val (Values a b) where
  value (Values x) = value x

values :: forall a b. (Val b) => Field a -> b -> Values a b
values a b = Values $ Tuple a b

infix 0 values as ~>
