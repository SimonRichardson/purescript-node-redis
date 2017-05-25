module Database.Redis.Commands.Property where

import Data.Array
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.NonEmpty as NE
import Data.Profunctor.Strong
import Data.Tuple

import Database.Redis.Commands.String

import Prelude

data Plain = Plain String

instance isStringPlain :: IsString Plain where
  fromString = Plain

instance semigroupPlain :: Semigroup Plain where
  append (Plain x) (Plain y) = Plain $ x <> y
  
instance monoidPlain :: Monoid Plain where
  mempty = Plain mempty

plain :: Plain -> String
plain (Plain p) = p

newtype Key a = Key Plain

instance isStringKey :: IsString (Key a) where
  fromString = Key <<< fromString

cast :: forall a. Key a -> Key Unit
cast (Key k) = Key k

newtype Value = Value Plain

instance isStringValue :: IsString Value where
  fromString = Value <<< fromString

instance semigroupValue :: Semigroup Value where
  append (Value a) (Value b) = Value $ a <> b

instance monoidValue :: Monoid Value where
  mempty = Value mempty

class Val a where
  value :: a -> (Array Value)

instance valString :: Val String where
  value = singleton <<< fromString

instance valValue :: Val Value where
  value = singleton

instance valTuple :: (Val a, Val b) => Val (Tuple a b) where
  value (Tuple a b) = value a <> value b

instance valNumber :: Val Number where
  value = singleton <<< fromString <<< show

instance valInt :: Val Int where
  value = singleton <<< fromString <<< show

instance valList :: (Val a) => Val (Array a) where
  value = comp

instance valNonEmpty :: (Val a) => Val (NE.NonEmpty Array a) where
  value = value <<< NE.oneOf

comp :: forall a. (Val a) => (Array a) -> (Array Value)
comp = concatMap value
