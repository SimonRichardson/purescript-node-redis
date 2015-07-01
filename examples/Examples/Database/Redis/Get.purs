module Examples.Database.Redis.Get where

import Database.Redis.Redis
import Database.Redis.Commands.Commands
import Database.Redis.Commands.Field 
import Database.Redis.Commands.Program
import Database.Redis.Commands.Render

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception

import Data.Either
import Data.Event
import Data.Maybe
import Data.Tuple.Nested

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

  x <- query commands database
  liftEff $ traceAny x

  close database

commands :: Query
commands = do
  set (name "list") "hello"
  get $ name "list"  
  mset [tuple2 (name "x") "y", tuple2 (name "a") "b"]
  mget [name "x", name "a"]