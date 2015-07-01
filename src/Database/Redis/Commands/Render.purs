module Database.Redis.Commands.Render where

import Data.Array
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Tuple
import qualified Data.Array.NonEmpty as NEL

import Database.Redis.Commands.Program
import Database.Redis.Commands.Property

newtype Inline = Inline [[String]]

getInline :: Inline -> [[String]]
getInline (Inline s) = s

instance semigroupInline :: Semigroup Inline where
  (<>) (Inline a) (Inline b) = Inline (a <> b)

instance monoidInline :: Monoid Inline where
  mempty = Inline mempty

type Rendered = Maybe Inline

render :: forall a. QueryM a -> Rendered
render = rules <<< runS

renderedInline :: Rendered -> Maybe [[String]]
renderedInline (Just x) = Just $ getInline x
renderedInline _        = Nothing

rules :: [Rule] -> Rendered
rules rs = rule' (mapMaybe property rs)
  where 
    property (Property k v) = Just (Tuple k v)
    property _              = Nothing

rule' :: forall a. [Tuple (Key a) [Value]] -> Rendered
rule' props = (Inline <<< properties <<< NEL.toArray) <$> nel (props >>= collect)

collect :: forall a. Tuple (Key a) [Value] -> [Either String (Tuple String [String])]
collect (Tuple (Key ky) v1) = collect' ky (mapMaybe extract v1)
  where
    extract (Value v) = Just v

collect' :: Plain -> [Plain] -> [Either String (Tuple String [String])]
collect' (Plain k) v = [Right (Tuple k (mapMaybe extract v))]
  where
    extract (Plain v) = Just v

properties :: [Either String (Tuple String [String])] -> [[String]]
properties xs = sheetRules <$> xs
  where 
    sheetRules = either (\_ -> mempty) (\(Tuple k v) -> mconcat [[k] <> v])

nel :: forall a. [a] -> Maybe (NEL.NonEmpty a)
nel []     = Nothing
nel (x:xs) = Just $ x NEL.:| xs
