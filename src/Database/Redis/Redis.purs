module Database.Redis.Redis 
  ( DB()
  , Client()
  , AffClient(), AffResult(), AffUnit()
  , connect, connect'
  , close, close'
  , query, query'
  ) where

import Prelude

import Control.Monad.Aff (Aff(), makeAff, makeAff', Canceler(..), nonCanceler)
import Control.Monad.Eff (Eff(), kind Effect)
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Error.Class (throwError)

import Data.Array
import Data.Either
import Data.Function.Uncurried (Fn3(), runFn3, Fn4(), runFn4, Fn5(), runFn5, Fn6())
import Data.Maybe
import Data.URI (printURIRef, runParseURIRef)

import Database.Redis.Commands.Program
import Database.Redis.Commands.Render

import Text.Parsing.StringParser

-- | The effect type for DB request made with Redis
foreign import data DB :: Effect

foreign import data Client :: Type

type AffClient e   = Aff (db :: DB | e) Client
type AffResult e a = Aff (db :: DB | e) (Array a)
type AffUnit e        = Aff (db :: DB | e) Unit

-- | Makes a connection to the database.
connect :: forall e. String -> AffClient e
connect = makeAff' <<< connect'

-- | Close the connection to the database
close :: forall e. Client -> AffUnit e
close = makeAff' <<< close'

-- | Query the client
query :: forall e a. Query -> Client -> AffResult e a
query q c = makeAff' $ query' q c

-- | Run a request directly without using 'Aff'
connect' :: forall e
  .  String
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Client -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
connect' s eb cb = do
  case runParseURIRef s of 
    Left err -> runFn3 _handleParseFailure (err' err) ignoreCancel eb
    Right x  -> runFn4 _connect (printURIRef x) ignoreCancel eb cb
  where
    err' :: ParseError -> Error
    err' e = error $ show e

close' :: forall e
  .  Client
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Unit -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
close' client eb cb = runFn4 _close client ignoreCancel eb cb

query' :: forall e a
  .  Query
  -> Client
  -> (Error -> Eff (db :: DB | e) Unit)
  -> ((Array a) -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
query' q c eb cb = runFn5 _query q' c ignoreCancel eb cb
  where
    q' = fromMaybe ([Command "" []]) <<< renderedInline $ render q

-- | Always ignore the cancel.
ignoreCancel :: forall e a. a -> Canceler (db :: DB | e)
ignoreCancel _ = nonCanceler

-- | foreign imports
foreign import _connect :: forall e. Fn4 
  String
  (Client -> Canceler (db :: DB | e))
  (Error -> Eff (db :: DB | e) Unit)
  (Client -> Eff (db :: DB | e) Unit)
  (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _handleParseFailure :: forall e. Fn3 
  Error
  (Client -> Canceler (db :: DB | e))
  (Error -> Eff (db :: DB | e) Unit)
  (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _close :: forall e. Fn4 
  Client
  (Unit -> Canceler (db :: DB | e))
  (Error -> Eff (db :: DB | e) Unit)
  (Unit -> Eff (db :: DB | e) Unit)
  (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _query :: forall e a. Fn5
  (Array Command)
  Client
  (Client -> Canceler (db :: DB | e))
  (Error -> Eff (db :: DB | e) Unit)
  ((Array a) -> Eff (db :: DB | e) Unit)
  (Eff (db :: DB | e) (Canceler (db :: DB | e)))   

foreign import _ignoreCancel :: forall e a. Fn4 
  a
  Error
  (Error -> Eff (db :: DB | e) Unit)
  (Boolean -> Eff (db :: DB | e) Unit)
  (Eff (db :: DB | e) Unit)
