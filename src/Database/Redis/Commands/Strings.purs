module Database.Redis.Commands.Strings where

import Data.Array
import Data.Foldable
import Data.Tuple.Nested

import Database.Redis.Commands.Field
import Database.Redis.Commands.Program
import Database.Redis.Commands.Property
import Database.Redis.Commands.Range (Range(..), toArray)
import Database.Redis.Commands.String
import Database.Redis.Commands.Values

import Prelude ((<<<), ($))

append :: forall a b. (Val b) => Field a -> b -> Query
append a b = key (fromString "APPEND") <<< value $ tuple2 a b

bitcount :: forall a. Field a -> Range Int -> Query
bitcount a b = key (fromString "BITCOUNT") <<< value $ tuple2 a (toArray b)

decr :: forall a. Field a -> Query
decr = key (fromString "DECR")

decrBy :: forall a. Field a -> Int -> Query
decrBy a b = key (fromString "DECRBY") <<< value $ tuple2 a b

get :: forall a. Field a -> Query
get = key (fromString "GET")

getBit :: forall a. Field a -> Int -> Query
getBit a b = key (fromString "GETBIT") <<< value $ tuple2 a b

getRange :: forall a. Field a -> Range Int -> Query
getRange a b = key (fromString "GETRANGE") <<< value $ tuple2 a (toArray b)

getSet :: forall a b. (Val b) => Field a -> b -> Query
getSet a b = key (fromString "GETSET") <<< value $ tuple2 a b

incr :: forall a. Field a -> Query
incr = key (fromString "INCR")

incrBy :: forall a. Field a -> Int -> Query
incrBy a b = key (fromString "INCRBY") <<< value $ tuple2 a b

incrByFloat :: forall a. Field a -> Number -> Query
incrByFloat a b = key (fromString "INCRBYFLOAT") <<< value $ tuple2 a b

mGet :: forall a. (Array (Field a)) -> Query
mGet = key (fromString "MGET") <<< comp

mSet :: forall a b. (Val b) => (Array (Values a b)) -> Query
mSet =  key (fromString "MSET") <<< comp

mSetNX :: forall a b. (Val b) => (Array (Values a b)) -> Query
mSetNX =  key (fromString "MSETNX") <<< comp

pSetEx :: forall a b. (Val b) => Field a -> Int -> b -> Query
pSetEx a b c = key (fromString "PSETEX") <<< value $ tuple3 a b c

set :: forall a b. (Val b) => Field a -> b -> Query
set a b = key (fromString "SET") <<< value $ tuple2 a b

setBit :: forall a b. (Val b) => Field a -> Int -> Int -> Query
setBit a b c = key (fromString "SETBIT") <<< value $ tuple3 a b c

setEx :: forall a b. (Val b) => Field a -> Number -> b -> Query
setEx a b c = key (fromString "SETEX") <<< value $ tuple3 a b c

setNX :: forall a b. (Val b) => Field a -> b -> Query
setNX a b = key (fromString "SETNX") <<< value $ tuple2 a b

setRange :: forall a b. (Val b) => Field a -> b -> Query
setRange a b = key (fromString "SETRANGE") <<< value $ tuple2 a b

strLen :: forall a. Field a -> Query
strLen = key (fromString "STRLEN")
