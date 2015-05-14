module Database.Redis.Commands.Commands where

import Data.Tuple

import Database.Redis.Commands.Field
import Database.Redis.Commands.Program
import Database.Redis.Commands.Selector
import Database.Redis.Commands.String

get :: Selector
get = fromString "GET"

set :: Selector
set = fromString "SET"
