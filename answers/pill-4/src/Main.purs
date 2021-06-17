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
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))

type Input
  = Map String (Array String)

type Parser a
  = Tuple (Array String) (Input -> Either String a)

data Scheme
  = Http
  | Https

derive instance Eq Scheme

instance Show Scheme where
  show Http = "Http"
  show Https = "Https"

-- Server $SCHEME $HOST $PORT
data Server
  = Server Scheme String Int

derive instance Eq Server

instance Show Server where
  show (Server scheme host port) =
    "(Server " <> show scheme <> " " <> show host <> " " <> show port <> ")"

type Credentials
  = { login :: String, password :: String }

data Options
  = Options Server Credentials

derive instance Eq Options

instance Show Options where
  show (Options server creds) =
    "(Options " <> show server <> " " <> show creds <> ")"

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

intParser :: Array String -> String -> Parser Int
-- intParser :: String -> Input -> Either String Int
intParser helpText name =
  helpText
    /\ \input -> case Map.lookup name input of
        Just [ value ] -> case Int.fromString value of
          Just int -> Right int
          Nothing -> Left $ "Value of option \"" <> name <> "\" is not a valid integer"
        Just [] -> Left $ "Option \"" <> name <> "\" has no value"
        Just _ -> Left $ "Expected option \"" <> name <> "\" to have one value only"
        Nothing -> Left $ "Missing option \"" <> name <> "\""

stringParser :: Array String -> String -> Parser String
-- stringParser :: String -> Input -> Either String String
stringParser helpText name =
  helpText
    /\ \input -> case Map.lookup name input of
        Just [ value ] -> Right value
        Just [] -> Left $ "Option \"" <> name <> "\" has no value"
        Just _ -> Left $ "Expected option \"" <> name <> "\" to have one value only"
        Nothing -> Left $ "Missing option \"" <> name <> "\""

tupleParser :: forall a b. Parser a -> Parser b -> Parser (Tuple a b)
tupleParser (parserAHelpText /\ parserAFn) (parserBHelpText /\ parserBFn) =
  Array.concat [ parserAHelpText <> parserBHelpText ]
    /\ \input -> Tuple <$> parserAFn input <*> parserBFn input

schemeParser :: Array String -> String -> Parser Scheme
schemeParser helpText key =
  fst parser
    /\ \input -> case snd parser input of
        Right "http" -> Right Http
        Right "https" -> Right Https
        Right _ -> Left $ "Expected option \"" <> key <> "\" to have value \"http\" or \"https\"."
        Left e -> Left e
  where
  parser = stringParser helpText key

serverParser :: Parser Scheme -> Parser String -> Parser Int -> Parser Server
serverParser schemeParser hostParser portParser =
  Array.concat [ fst schemeParser, fst hostParser, fst portParser ]
    /\ \input -> do
        scheme <- snd schemeParser input
        host <- snd hostParser input
        port <- snd portParser input
        pure $ Server scheme host port

-- Another version
serverParser1 :: Parser Scheme -> Parser String -> Parser Int -> Parser Server
serverParser1 (schemeHelpText /\ schemeParserFn) (hostHelpText /\ hostParserFn) (portHelpText /\ portParserFn) =
  Array.concat [ schemeHelpText, hostHelpText, portHelpText ]
    /\ \input ->
        Server
          <$> schemeParserFn input
          <*> hostParserFn input
          <*> portParserFn input

credentialsParser :: Parser String -> Parser String -> Parser Credentials
credentialsParser (loginHelpText /\ loginParserFn) (passwordHelpText /\ passwordParserFn) =
  Array.concat [ loginHelpText, passwordHelpText ]
    /\ \input ->
        { login: _, password: _ }
          <$> loginParserFn input
          <*> passwordParserFn input

optionsParser :: Parser Server -> Parser Credentials -> Parser Options
optionsParser (schemeHelpText /\ schemeParserFn) (credentialsHelpText /\ credentialsParserFn) =
  Array.concat [ schemeHelpText, credentialsHelpText ]
    /\ \input ->
        Options
          <$> schemeParserFn input
          <*> credentialsParserFn input

data Result a
  = HelpText (Array String)
  | Error String (Array String)
  | Success a

instance Show a => Show (Result a) where
  show = case _ of
    HelpText helpText -> "(HelpText " <> show helpText <> ")"
    Error err helpText -> "(Error " <> show err <> show helpText <> ")"
    Success a -> "(Success " <> show a <> ")"

runParser :: forall a. Parser a -> Input -> Result a
runParser (helpText /\ parserFn) input =
  if Map.member "help" input then
    HelpText helpText
  else case parserFn input of
    Right a -> Success a
    Left err -> Error err helpText
