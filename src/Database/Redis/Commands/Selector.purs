module Database.Redis.Commands.Selector where

import Database.Redis.Commands.String
import qualified Data.String as S

data Path = Elem String
          | Deep Selector Selector

data Selector = Selector Path

instance isStringSelector :: IsString Selector where
  fromString s = case S.take 1 s of
    _ -> Selector (Elem s)

deep :: Selector -> Selector -> Selector
deep a b = Selector (Deep a b)

(**) :: Selector -> Selector -> Selector
(**) = deep    