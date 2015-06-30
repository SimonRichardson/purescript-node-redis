module Database.Redis.Commands.Field where 

import Database.Redis.Commands.Property
import Database.Redis.Commands.String
import Data.Tuple.Nested

newtype Field a = Field Value

instance valField :: Val (Field a) where
  value (Field v) = v

data Name

name :: String -> Field Name
name n = Field $ fromString n

field :: String -> String -> Field Name
field a b = Field $ (fromString a <> fromString "->" <> fromString b)

newtype Limit = Limit Value

instance valLimit :: Val Limit where
  value (Limit v) = v

range :: Number -> Number -> Limit
range a b = Limit $ value (tuple2 a b)
