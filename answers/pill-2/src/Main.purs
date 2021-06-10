module Main where

import Prelude

import Data.Array (foldM)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)

type Input
  = Map String (Array String)

type Parser a
  = Input -> Either String a

intParser :: String -> Parser Int
intParser name input = case Map.lookup name input of
  Just [ value ] -> case Int.fromString value of
    Just int -> Right int
    Nothing -> Left $ "Value of option \"" <> name <> "\" is not a valid integer"
  Just [] -> Left $ "Option \"" <> name <> "\" has no value"
  Just _ -> Left $ "Expected option \"" <> name <> "\" to have one value only"
  Nothing -> Left $ "Missing option \"" <> name <> "\""

stringParser :: String -> Parser String
stringParser name input = case Map.lookup name input of
  Just [ value ] -> Right value
  Just [] -> Left $ "Option \"" <> name <> "\" has no value"
  Just _ -> Left $ "Expected option \"" <> name <> "\" to have one value only"
  Nothing -> Left $ "Missing option \"" <> name <> "\""

tupleParser :: forall a b. Parser a -> Parser b -> Parser (Tuple a b)
tupleParser parserA parserB input = Tuple <$> parserA input <*> parserB input

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
