module Database.Redis.Commands.Program where

import Control.Monad.Writer
import Control.Monad.Writer.Class

import Data.Maybe
import Data.Profunctor.Strong
import Data.Tuple
import qualified Data.Array.NonEmpty as NEL

import Database.Redis.Commands.Property

data Rule = Property (Key Unit) Value

newtype QueryM a = S (Writer [Rule] a)

instance functorQueryM :: Functor QueryM where
  (<$>) f (S w) = S $ f <$> w

instance applyQueryM :: Apply QueryM where
  (<*>) (S f) (S w) = S $ f <*> w

instance bindQueryM :: Bind QueryM where
  (>>=) (S w) f = S $ w >>= (\(S w') -> w') <<< f

instance applicativeQueryM :: Applicative QueryM where
  pure = S <<< pure

instance monadQueryM :: Monad QueryM

runS :: forall a. QueryM a -> [Rule]
runS (S s) = execWriter s

rule :: Rule -> Query
rule = S <<< tell <<< (:[])

type Query = QueryM Unit

key :: forall a. (Val a) => Key a -> a -> Query
key k v = rule $ Property (cast k) (value v)
