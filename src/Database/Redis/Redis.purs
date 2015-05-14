module Database.Redis.Redis 
  ( DB()
  , Client()
  , AffClient(), AffResult(), AffUnit()
  , connect, connect'
  , close, close'
  , query, query'
  ) where

import Control.Monad.Aff (Aff(), makeAff, makeAff', Canceler(..), nonCanceler)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Error.Class (throwError)

import Data.Either
import Data.Function (Fn3(), runFn3, Fn4(), runFn4, Fn5(), runFn5, Fn6())
import Data.Maybe
import Data.URI

import Database.Redis.Commands.Program
import Database.Redis.Commands.Render

import Text.Parsing.StringParser

-- | The effect type for DB request made with Redis
foreign import data DB :: !

foreign import data Client :: *

type AffClient e   = Aff (db :: DB | e) Client
type AffResult e a = Aff (db :: DB | e) a
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
  case parseFileURI s of 
    Left err -> runFn3 _handleParseFailure (err' err) ignoreCancel eb
    Right x  -> runFn4 _connect (printURI x) ignoreCancel eb cb
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
  -> (a -> Eff (db :: DB | e) Unit)
  -> (Eff (db :: DB | e) (Canceler (db :: DB | e)))
query' q c eb cb = runFn5 _query q' c ignoreCancel eb cb
  where
    q' = fromMaybe "" <<< renderedInline $ render q

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

foreign import _close
  """
  function _close(client, canceler, errback, callback) {
    process.nextTick(function() {
      client.end();
      callback({})();
    });
    return canceler({});
  }
  """ :: forall e. Fn4 
                   Client
                   (Unit -> Canceler (db :: DB | e))
                   (Error -> Eff (db :: DB | e) Unit)
                   (Unit -> Eff (db :: DB | e) Unit)
                   (Eff (db :: DB | e) (Canceler (db :: DB | e)))

foreign import _query
  """
  function _query(query, client, canceler, errback, callback) {
    console.log("Query:", query);
    var Command = require('ioredis/lib/command'),
        options = {
          replyEncoding: 'utf8'
        },
        command = new Command(query[0], query.slice(1), options, function(err, x) {
          (err ? errback(err) : callback(x))();
        });
    client.sendCommand(command);
    return canceler(client);
  }
  """ :: forall e a.  Fn5
                      String
                      Client
                      (Client -> Canceler (db :: DB | e))
                      (Error -> Eff (db :: DB | e) Unit)
                      (a -> Eff (db :: DB | e) Unit)
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
