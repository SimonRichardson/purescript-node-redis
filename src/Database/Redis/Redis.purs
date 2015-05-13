module Database.Redis.Redis 
  ( DB()
  , Client()
  , AffClient()
  , connect, connect'
  ) where

import Control.Monad.Aff (Aff(), makeAff, makeAff', Canceler(..), nonCanceler)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Error.Class (throwError)

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject)
import Data.Argonaut.Core (Json())
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either
import Data.Function (Fn3(), runFn3, Fn4(), runFn4, Fn5(), runFn5, Fn6(), runFn6, Fn7(), runFn7, Fn8(), runFn8)
import Data.Maybe
import Data.URI

import Text.Parsing.StringParser

-- | The effect type for DB request made with Redis
foreign import data DB :: !

foreign import data Client :: *

type AffClient e = Aff (db :: DB | e) Client

-- | Makes a connection to the database.
connect :: forall e. String -> AffClient e
connect = makeAff' <<< connect'

-- | Run a request directly without using 'Aff'
connect' :: forall e
  .  String
  -> (Error -> Eff (db :: DB | e) Unit)
  -> (Client -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
connect' s eb cb = do
  case parseFileURI s of 
    Left err -> runFn3 _handleParseFailure (err' err) ignoreCancel eb
    Right x  -> runFn4 _connect (printURI x) ignoreCancel eb cb
  where
    err' :: ParseError -> Error
    err' e = error $ show e

-- | Always ignore the cancel.
ignoreCancel :: forall e a. a -> Canceler (db :: DB | e)
ignoreCancel _ = nonCanceler

-- | foreign imports
foreign import _connect
  """
  function _connect(uri, canceler, errback, callback) {
    var Redis = require('ioredis'),
        client = new Redis({lazyConnect: true});
    client.options.enableReadyCheck = true;
    client.parseOptions(uri);
    client.connect();
    client.once('ready', function(err, _) {
        (err ? errback(err) : callback(client))();
    });
    return canceler(client);
  }
  """ :: forall e. Fn4 
                   String
                   (Client -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Client -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _handleParseFailure
  """
  function _handleParseFailure(err, canceler, errback) {
    process.nextTick(function() {
      errback(err)();
    });
    var Redis = require('ioredis'),
        client = new Redis({lazyConnect: true});
    return canceler(client);
  }
  """ :: forall e. Fn3 
                   Error
                   (Client -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _ignoreCancel
  """
  function _ignoreCancel(any, cancelError, errback, callback) {
    return function() {
        callback(false);
    };
  }
  """ :: forall e a. Fn4 
                     a
                     Error
                     (Error -> Eff (db :: DB | e) Unit)
                     (Boolean -> Eff (db :: DB | e) Unit)
                     (Eff (db :: DB | e) Unit)
