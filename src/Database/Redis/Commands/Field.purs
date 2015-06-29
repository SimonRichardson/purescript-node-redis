module Database.Redis.Commands.Field where 

import Database.Redis.Commands.Property
import Database.Redis.Commands.String

newtype Field a = Field Value

instance valField :: Val (Field a) where
  value (Field v) = v

data Name

name :: String -> Field Name
name n = Field $ fromString n