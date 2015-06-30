module Database.Redis.Commands.Commands where

import Data.Foldable
import Data.Tuple.Nested

import Database.Redis.Commands.Field
import Database.Redis.Commands.Program
import Database.Redis.Commands.Property
import Database.Redis.Commands.String

get :: forall a. Field a -> Query
get = key (fromString "GET")

set :: forall a b. (Val b) => Field a -> b -> Query
set a b = key (fromString "SET") <<< value $ tuple2 a b

mget :: forall a. [Field a] -> Query
mget = key (fromString "MGET") <<< noCommas

mset :: forall a b. (Val b) => [Tuple2 (Field a) b] -> Query
mset =  key (fromString "MSET") <<< noCommas