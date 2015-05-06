module Database.Redis.Redis 
  ( DB()
  ) where

import Control.Monad.Aff (Aff(), makeAff, makeAff', Canceler(..))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Error.Class (throwError)

import Data.Either
import Data.Function (Fn4(), runFn4, Fn5(), runFn5, Fn6(), runFn6, Fn7(), runFn7, Fn8(), runFn8)
import Data.Maybe

-- | The effect type for DB request made with Redis
foreign import data DB :: !
