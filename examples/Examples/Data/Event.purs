module Data.Event where


import Data.Date
import Data.Either
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)

import Debug.Trace

newtype Event = Event
  { name :: Maybe String
  }

instance showEvent :: Show Event where
  show (Event e) = "Event " ++
    "{ name: " ++ show e.name ++
    "}"
  