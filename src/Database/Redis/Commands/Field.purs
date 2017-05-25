module Database.Redis.Commands.Field where 

import Database.Redis.Commands.Property (class Val, Value)
import Database.Redis.Commands.String (fromString)

import Data.Array (singleton)

import Prelude (($), (<>))

newtype Field a = Field Value

instance valField :: Val (Field a) where
  value (Field v) = singleton v

data Name

name :: String -> Field Name
name n = Field $ fromString n

field :: String -> String -> Field Name
field a b = Field $ (fromString a <> fromString "->" <> fromString b)
