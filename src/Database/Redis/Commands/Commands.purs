module Database.Redis.Commands.Commands where

import Data.Tuple

import Database.Redis.Commands.Field
import Database.Redis.Commands.Program
import Database.Redis.Commands.String

get :: forall a. Field a -> Query
get = key (fromString "GET")

set :: forall a. Field a -> Query
set = key (fromString "SET")
