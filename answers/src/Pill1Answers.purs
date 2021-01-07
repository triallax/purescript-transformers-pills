module Pill1Answers where

import Prelude
import Data.Array (foldM)
import Data.Array as Array
import Data.Int as Int
import Data.String as String
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Pill1Types (Parser, Input)

intParser :: String -> Parser Int
-- intParser :: String -> Input -> Maybe Int
intParser name input = case Map.lookup name input of
  Just [ value ] -> Int.fromString value
  _ -> Nothing

stringParser :: String -> Parser String
-- stringParser :: String -> Input -> Maybe String
stringParser name input = case Map.lookup name input of
  Just [ value ] -> Just value
  _ -> Nothing

-- Don't worry; this isn't the only solution. :)
tupleParser :: forall a b. Parser a -> Parser b -> Parser (Tuple a b)
tupleParser parserA parserB input = Tuple <$> parserA input <*> parserB input

-- Brace yourselves.
type FoldAcc
  = { acc :: Input
    , prev :: Maybe { key :: String, values :: Array String }
    }

fold :: Array String -> Maybe Input
fold [] = Just Map.empty

fold args = do
  { acc, prev } <-
    foldM go { acc: Map.empty, prev: Nothing } args
  { values, key } <- prev
  pure $ Map.insert key values acc
  where
  go :: FoldAcc -> String -> Maybe FoldAcc
  go { acc, prev } curr = do
    let
      maybeCurrKey = String.stripPrefix (String.Pattern "--") curr
    case maybeCurrKey, prev of
      Just key, Nothing -> Just { acc, prev: Just { key, values: [] } }
      Just currKey, Just { key: prevKey, values } ->
        Just
          { prev: Just { key: currKey, values: [] }
          , acc: Map.insert prevKey values acc
          }
      Nothing, Just { key, values } ->
        Just
          { acc
          , prev: Just { key, values: Array.snoc values curr }
          }
      Nothing, Nothing -> Nothing
