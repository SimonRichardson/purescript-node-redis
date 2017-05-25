module Database.Redis.Commands.Render where

import Data.Array
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.NonEmpty as NE
import Data.Tuple

import Database.Redis.Commands.Program
import Database.Redis.Commands.Property

import Prelude

data Command = Command String (Array String)

instance semigroupCommand :: Semigroup Command where
  append (Command a b) (Command x y) = Command (a <> x) (b <> y)

instance monoidCommand :: Monoid Command where
  mempty = Command mempty mempty  

newtype Inline = Inline (Array Command)

getInline :: Inline -> (Array Command)
getInline (Inline s) = s

instance semigroupInline :: Semigroup Inline where
  append (Inline a) (Inline b) = Inline (a <> b)

instance monoidInline :: Monoid Inline where
  mempty = Inline mempty

type Rendered = Maybe Inline

render :: forall a. QueryM a -> Rendered
render = rules <<< runS

renderedInline :: Rendered -> Maybe (Array Command)
renderedInline (Just x) = Just $ getInline x
renderedInline _        = Nothing

rules :: (Array Rule) -> Rendered
rules rs = rule' (mapMaybe property rs)
  where 
    property (Property k v) = Just (Tuple k v)
    property _              = Nothing

rule' :: forall a. (Array (Tuple (Key a) (Array Value))) -> Rendered
rule' props = (Inline <<< properties <<< NE.oneOf) <$> nel (props >>= collect)

collect :: forall a. Tuple (Key a) (Array Value) -> (Array (Either String (Tuple String (Array String))))
collect (Tuple (Key ky) v1) = collect' ky (mapMaybe extract v1)
  where
    extract (Value v) = Just v

collect' :: Plain -> (Array Plain) -> (Array (Either String (Tuple String (Array String))))
collect' (Plain k) v = [Right (Tuple k (mapMaybe extract v))]
  where
    extract (Plain v) = Just v

properties :: (Array (Either String (Tuple String (Array String)))) -> (Array Command)
properties xs = sheetRules <$> xs
  where 
    sheetRules = either (\_ -> mempty) (\(Tuple k v) -> fold [Command k v])

nel :: forall a. (Array a) -> Maybe (NE.NonEmpty Array a)
nel arr = (uncons arr) >>= (\x -> Just $ x.head NE.:| x.tail)
