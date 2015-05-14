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
import Database.Redis.Commands.Selector

newtype Inline = Inline String

getInline :: Inline -> String
getInline (Inline s) = s

instance semigroupInline :: Semigroup Inline where
  (<>) (Inline a) (Inline b) = Inline (a <> b)

instance monoidInline :: Monoid Inline where
  mempty = Inline mempty

type Rendered = Maybe Inline

render :: forall a. QueryM a -> Rendered
render = rules [] <<< runS

renderedInline :: Rendered -> Maybe String
renderedInline x = case x of
  Just a -> Just $ getInline a
  _      -> Nothing

rules :: [App] -> [Rule] -> Rendered
rules sel rs = nestedSheets
  where 
    property (Property k v) = Just (Tuple k v)
    property _              = Nothing

    nested   (Nested a ns ) = Just (Tuple a ns)
    nested   _              = Nothing

    --topRules      = rule' sel (mapMaybe property rs)

    nestedSheets  = foldMap (<> (Just $ Inline "\n")) $ uncurry nestedRules <$> mapMaybe nested rs
    nestedRules a = rules (a : sel)

rule' :: forall a. [App] -> [Tuple (Key a) Value] -> Rendered
rule' sel props = maybe q o $ nel sel
  where 
    p = props >>= collect
    q =  (Inline <<< properties <<< NEL.toArray) <$> nel p
    o sel' = Just <<< Inline $ intercalate " " [selector (merger sel'), "{", properties p, "}"]

selector :: Selector -> String
selector = intercalate ", " <<< selector'

selector' :: Selector -> [String]
selector' (Selector p) = selector'' p

selector'' :: Path -> [String]
selector'' (Elem t) = [t]
selector'' (Deep a b) = sepWith " " <$> selector' a <*> selector' b

sepWith :: String -> String -> String -> String
sepWith s a b = a <> s <> b

collect :: forall a. Tuple (Key a) Value -> [Either String (Tuple String String)]
collect (Tuple (Key ky) (Value v1)) = collect' ky v1

collect' :: Prefixed -> Prefixed -> [Either String (Tuple String String)]
collect' (Plain k) (Plain v)         = [Right (Tuple k v)]
collect' (Prefixed ks) (Plain v)     = (\(Tuple p k) -> Right $ Tuple (p <> k) v) <$> ks
collect' (Plain k) (Prefixed vs)     = (\(Tuple p v) -> Right $ Tuple k (p <> v)) <$> vs
collect' (Prefixed ks) (Prefixed vs) = (\(Tuple p k) -> maybe (Left (p <> k)) (Right <<< Tuple (p <> k) <<< (p <>)) $ lookup p vs) <$> ks

properties :: [Either String (Tuple String String)] -> String
properties xs = intercalate "; " $  sheetRules <$> xs
  where 
    sheetRules = either (\_ -> mempty) (\(Tuple k v) -> mconcat [k, ": ", v])

merger :: NEL.NonEmpty App -> Selector
merger (NEL.NonEmpty x xs) =
  case x of
    Sub s   -> maybe s (\xs' -> merger xs' ** s) $ nel xs
    Root s  -> maybe s (\xs' -> s ** merger xs') $ nel xs

nel :: forall a. [a] -> Maybe (NEL.NonEmpty a)
nel []     = Nothing
nel (x:xs) = Just $ x NEL.:| xs
