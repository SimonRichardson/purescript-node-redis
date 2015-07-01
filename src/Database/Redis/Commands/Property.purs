module Database.Redis.Commands.Property where

import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Profunctor.Strong
import Data.Tuple
import Data.Array
import qualified Data.Array.NonEmpty as NEL

import Database.Redis.Commands.String

data Plain = Plain String

instance isStringPlain :: IsString Plain where
  fromString = Plain

instance semigroupPlain :: Semigroup Plain where
  (<>) (Plain x) (Plain y) = Plain $ x <> y
  
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
  (<>) (Value a) (Value b) = Value $ a <> b

instance monoidValue :: Monoid Value where
  mempty = Value mempty

class Val a where
  value :: a -> [Value]

instance valString :: Val String where
  value = singleton <<< fromString

instance valValue :: Val Value where
  value = singleton

instance valTuple :: (Val a, Val b) => Val (Tuple a b) where
  value (Tuple a b) = value a <> value b

instance valNumber :: Val Number where
  value = singleton <<< fromString <<< show

instance valList :: (Val a) => Val [a] where
  value = comp

instance valNonEmpty :: (Val a) => Val (NEL.NonEmpty a) where
  value = value <<< NEL.toArray

comp :: forall a. (Val a) => [a] -> [Value]
comp = concatMap value
