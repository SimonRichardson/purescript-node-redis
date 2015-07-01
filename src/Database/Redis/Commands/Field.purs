module Database.Redis.Commands.Field where 

import Database.Redis.Commands.Property
import Database.Redis.Commands.String

import Data.Array
import Data.Tuple.Nested

newtype Field a = Field Value

instance valField :: Val (Field a) where
  value (Field v) = singleton v

data Name

name :: String -> Field Name
name n = Field $ fromString n

field :: String -> String -> Field Name
field a b = Field $ (fromString a <> fromString "->" <> fromString b)
