module Database.Redis.Commands.Field where

import Database.Redis.Commands.Property
import Database.Redis.Commands.Program
import Database.Redis.Commands.String

name :: String -> Query
name = key $ fromString "name"