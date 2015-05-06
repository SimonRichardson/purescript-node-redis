module Examples.Database.Redis.Find where

import Database.Redis.Redis
import Database.Redis.ConnectionInfo
import Database.Redis.Bson.BsonValue

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception

import Data.Either
import Data.Event
import Data.Maybe

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

main = launchAff $ do
  liftEff $ uint
