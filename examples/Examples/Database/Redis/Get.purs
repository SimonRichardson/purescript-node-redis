module Examples.Database.Redis.Get where

import Database.Redis.Redis
import Database.Redis.Commands.Field 
import Database.Redis.Commands.Program
import Database.Redis.Commands.Render
import Database.Redis.Commands.Strings
import Database.Redis.Commands.Values

import Control.Monad.Aff
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console

import Data.Either
import Data.Maybe
import Data.Tuple.Nested

import Prelude (($), bind, Unit(..))

foreign import logAny :: forall e a. a -> Eff (console :: CONSOLE | e) Unit

uri :: String
uri = "redis://127.0.0.1:6379"

main = launchAff $ do
  Right database <- attempt $ connect uri

  x <- query commands database
  liftEff $ logAny x

  close database

commands :: Query
commands = do
  set (name "list") "hello"
  get $ name "list"  
  mSet [(name "x") ~> "y", (name "a") ~> "b"]
  mGet [name "x", name "a"]
