module Examples.Database.Redis.Get where

import Database.Redis.Redis
import Database.Redis.Commands

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception

import Data.Array
import Data.Either
import Data.Event
import Data.Maybe
import Data.Tuple

import Debug.Trace

foreign import traceAny
  """
  function traceAny(a){
    return function () {
      console.log(a);
      return {};
    };
  }
  """ :: forall e a. a -> Eff (trace :: Trace | e) Unit

uri :: String
uri = "redis://127.0.0.1:6379"

main = launchAff $ do
  Right database <- attempt $ connect uri

  query (MSet [Tuple "key" "value", Tuple "key" "value"]) database
  res <- query (Sort "list" 
    [ BY $ FIELD "hash:*" "field"
    , LIMIT 2 3
    , GET $ NAME "gk"
    , GET $ NAME "#"
    , GET $ FIELD "gh" "f*"
    , DESC
    , ALPHA
    , STORE $ NAME "store"
    ]) database
  liftEff $ traceAny res

  close database
