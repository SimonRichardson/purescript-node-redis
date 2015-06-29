module Database.Redis.Commands.Field where 

import Database.Redis.Commands.Property
import Database.Redis.Commands.String

newtype Field a = Field Value

instance valField :: Val (Field a) where
  value (Field v) = v

data Name
data Prop

name :: String -> Field Name
name n = Field $ fromString n

field :: String -> String -> Field Prop
field a b = Field $ (fromString a <> fromString "->" <> fromString b)