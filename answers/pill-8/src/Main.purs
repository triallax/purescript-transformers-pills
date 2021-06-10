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
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V, invalid, toEither)

newtype Compose f g a
  = Compose (f (g a))

instance (Functor f, Functor g) => Functor (Compose f g) where
  map f (Compose fga) = Compose $ map (map f) fga

instance (Apply f, Apply g) => Apply (Compose f g) where
  apply (Compose fga2b) (Compose fga) = Compose (apply <$> fga2b <*> fga)

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose <<< pure <<< pure

type Input
  = Map String (Array String)

type Parser = Compose (Tuple (Array String)) (Compose (Function Input) (V (Array String)))

mkParser :: forall a. Array String -> (Input -> V (Array String) a) -> Parser a
mkParser helpText f = Compose $ helpText /\ Compose f

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
  mkParser helpText \input -> case Map.lookup name input of
        Just [ value ] -> case Int.fromString value of
          Just int -> pure int
          Nothing -> invalid [ "Value of option \"" <> name <> "\" is not a valid integer" ]
        Just [] -> invalid [ "Option \"" <> name <> "\" has no value" ]
        Just _ -> invalid [ "Expected option \"" <> name <> "\" to have one value only" ]
        Nothing -> invalid [ "Missing option \"" <> name <> "\"" ]

stringParser :: Array String -> String -> Parser String
-- stringParser :: String -> Input -> Either String String
stringParser helpText name =
  mkParser helpText \input -> case Map.lookup name input of
        Just [ value ] -> pure value
        Just [] -> invalid ["Option \"" <> name <> "\" has no value"]
        Just _ -> invalid [ "Expected option \"" <> name <> "\" to have one value only" ]
        Nothing -> invalid [ "Missing option \"" <> name <> "\"" ]

-- This parser, as well as all of the following parsers which can be composed,
-- can also be written using `liftN`. For example, this function can be defined
-- as just `serverParser = lift2 Tuple`. See the `liftN` functions here:
-- https://pursuit.purescript.org/packages/purescript-prelude/5.0.1/docs/Control.Apply
tupleParser :: forall a b. Parser a -> Parser b -> Parser (Tuple a b)
tupleParser parserA parserB = Tuple <$> parserA <*> parserB

schemeParser :: Array String -> String -> Parser Scheme
schemeParser helpText key =
  mkParser helpText \input -> case toEither $ parserFn input of
        Right "http" -> pure Http
        Right "https" -> pure Https
        Right _ -> invalid [ "Expected option \"" <> key <> "\" to have value \"http\" or \"https\"." ]
        Left e -> invalid e
  where
  Compose (helpText /\ Compose parserFn) = stringParser helpText key

serverParser :: Parser Scheme -> Parser String -> Parser Int -> Parser Server
serverParser schemeParser hostParser portParser =
    Server <$> schemeParser <*> hostParser <*> portParser

credentialsParser :: Parser String -> Parser String -> Parser Credentials
credentialsParser loginParser passwordParser =
  { login: _, password: _ } <$> loginParser <*> passwordParser

optionsParser :: Parser Server -> Parser Credentials -> Parser Options
optionsParser schemeParser credentialsParser =
  Options <$> schemeParser <*> credentialsParser

data Result a
  = HelpText (Array String)
  | Error (Array String) (Array String)
  | Success a

instance Show a => Show (Result a) where
  show = case _ of
    HelpText helpText -> "(HelpText " <> show helpText <> ")"
    Error err helpText -> "(Error " <> show err <> " " <> show helpText <> ")"
    Success a -> "(Success " <> show a <> ")"

runParser :: forall a. Parser a -> Input -> Result a
runParser (Compose (helpText /\ Compose parserFn)) input =
  if Map.member "help" input then
    HelpText helpText
  else case toEither $ parserFn input of
    Right a -> Success a
    Left err -> Error err helpText
