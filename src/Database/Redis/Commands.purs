module Database.Redis.Commands where

import Data.Array
import Data.Foldable
import Data.Tuple

type Sort = [SortValue]

newtype CommandQuery = CommandQuery
  { name :: String
  , args :: [String] 
  }

data Command
  = Get     String
  | Set     String String
  | MGet    [String]
  | MSet    [Tuple String String]
  | Sort    String Sort

data SortValue
  = BY    SortKey
  | GET   SortKey
  | STORE SortKey
  | LIMIT Number Number
  | DESC
  | ALPHA

data SortKey
  = NAME  String
  | FIELD String String

instance showCommand :: Show Command where
  show (Get x)    = "Get " ++ show x
  show (Set x y)  = "Set " ++ show x ++ " " ++ show y
  show (MGet x)   = "MGet " ++ show x
  show (MSet x)   = "MSet " ++ showPairs x
  show (Sort x y) = "Sort " ++ show x ++ " " ++ show y

instance showSortValue :: Show SortValue where 
  show (BY x)      = " BY" ++ show x
  show (GET x)     = " GET" ++ show x
  show (LIMIT x y) = " LIMIT" ++ show x ++ " " ++ show y
  show (STORE x)   = " STORE" ++ show x
  show DESC        = " DESC"
  show ALPHA       = " ALPHA"

instance showSortKey :: Show SortKey where 
  show (NAME x)    = x
  show (FIELD x y) = x ++ "->" ++ y

showPairs :: [Tuple String String] -> String
showPairs a = showPairs' a ""
  where 
    showPairs' :: [Tuple String String] -> String -> String
    showPairs' (Tuple a b : xs) s  = "(" ++ a ++ ": " ++ b ++ ") " ++ showPairs' xs s
    showPairs' _ s                 = s

runCommand :: Command -> CommandQuery
runCommand = extract
  where
    extract :: Command -> CommandQuery
    extract (Get a)       = go "get"  [a]
    extract (Set a b)     = go "set"  [a, b]
    extract (MGet a)      = go "mget" a
    extract (MSet a)      = go "mset" $ toArray a []
    extract (Sort a b)    = go "sort" $ [a] <> flatten b

    toArray :: [Tuple String String] -> [String] -> [String]
    toArray (Tuple a b : xs) s = [a, b] <> toArray xs s
    toArray _ s                = s

    toArray' :: SortValue -> [String]
    toArray' (BY a)      = ["BY", show a]
    toArray' (GET a)     = ["GET", show a]
    toArray' (LIMIT a b) = ["LIMIT", show a, show b]
    toArray' (STORE a)   = ["STORE", show a]
    toArray' DESC        = ["DESC"]
    toArray' ALPHA       = ["ALPHA"]

    flatten :: Sort -> [String]
    flatten = foldl (\acc n -> acc <> toArray' n) []

    go :: String -> [String] -> CommandQuery
    go a b = CommandQuery
      { name : a
      , args : b
      }