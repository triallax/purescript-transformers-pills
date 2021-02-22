## PureScript Transformers Pills

**Note**: this is still very much incomplete.

This is a guide intended to teach the concept of monad transformers in bite-sized pieces (we call them "pills").

### Assumptions

We assume you, the reader, have a **basic** knowledge of PureScript, PureScript concepts (like `Functor`, `Applicative`, and `Bind`) and PureScript data types (like `Maybe`, `Either`, and `Map`).

### Licensing

All the code snippets are under [the 3-clause BSD license](https://opensource.org/licenses/BSD-3-Clause), while everything else is under the [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/).

### Pills

<!-- TODO: move pills to their own files -->

#### Pill 1

The general plan is to build a rudimentary command-line option parser and then expand it using monad transformers to add extra features and get cleaner code.

We expect our minimum viable product (MVP) to be able to parse basic command-line options: `ps-client --scheme https --host example.com --port 8000 --login admin --password hunter2`.

Let's model our problem domain (I always wanted to say that):

```purescript
type Input = Map String (Array String)
```

`String` is the option name (e.g. `host`, with the double hypen removed), and `Array String` contains the values following the option.

The representation of the options in our command example would look like this:

```purescript
exampleInput = Map.fromFoldable [ Tuple "scheme" ["https"], Tuple "host" ["example.com"], Tuple "port" ["8000"], Tuple "login" ["admin"], Tuple "password" ["hunter2"] ]
```

We'll need to transform `Input` into well-typed values (`Int`, `String`, `Bool`, etc.). That's the purpose of `Parser`:

```purescript
type Parser a = Input -> Maybe a
```

Exercises:

1. Please write a function `intParser :: String -> Parser Int`. `String` here represents the option name. Check that these tests run correctly:

```purescript
intParserTest1 = assertEquals (intParser "port" exampleInput) (Just 8000)

intParserTest2 = assertEquals (intParser "pord" exampleInput) Nothing

intParserTest3 = assertEquals (intParser "host" exampleInput) Nothing

intParserTest4 = assertEquals (intParser "port" $ Map.fromFoldable [ "port" /\ [ "8000", "8080" ] ]) Nothing
```

2. Please write a function `stringParser :: String -> Parser String`. Check that these tests run correctly:

```purescript
stringParserTest1 = assertEquals (stringParser "host" exampleInput) (Just "example.com")

stringParserTest2 = assertEquals (stringParser "login" exampleInput) (Just "admin")

stringParserTest3 = assertEquals (stringParser "password" exampleInput) (Just "hunter2")

stringParserTest4 = assertEquals (stringParser "whatever" exampleInput) Nothing

stringParserTest5 = assertEquals (stringParser "host" $ Map.fromFoldable [ "host" /\ [ "example.com", "purescript.org" ] ]) Nothing
```

<!--
3. Please write a function `tupleParser :: forall a b. Parser a -> Parser b -> Parser (Tuple a b)`. Check that these tests run correctly:

 TODO: provide two solutions: one by hand and one using Apply

```purescript
credentialsParserTest = assertEquals (tupleParser (stringParser "login") (stringParser "password")) (Just (Tuple "admin" "hunter2"))

clientParserTest = assertEquals (tupleParser (stringParser "host") (intParser "port")) (Just (Tuple "example.com" 8000))

failedTupleTest = assertEquals (tupleParser (intParser "host") (stringParser "port")) Nothing
```
-->

4. (Only for the curious since it is not directly relevant to our topic) Please write a function `fold :: Array String -> Maybe Input`. `fold` should transform from an array of command-line options to `Input`. Check that these tests run correctly:

```purescript
foldTest1 = assertEquals
  (fold
    [ "--x", "1"
    , "--y", "string", "another string"
    , "--z"
    ]) $ Just $ Map.fromFoldable ["x" /\ ["2"], "y" /\ ["string", "another string"], "z" /\ [] ]

foldTest2 = assertEquals (fold ["x"]) Nothing

foldTest3 = assertEquals (fold []) $ Just mempty
```

#### Pill 2

You might have noticed in the previous pill's exercises that if a parser returns `Nothing`, there is no way to know _why_ it does. For example, taking a look at `intParser`, if we get `Nothing` back, there's no way to get any information about the cause of the failure (for example, a value might have not been provided, or it _was_ provided but wasn't a valid integer). Thinking about this abstractly, we want to somehow add error information to `Nothing` in the case of failure. PureScript already has exactly a type for this purpose, which is `Either`.

We can start by changing `Maybe` in `Either` in `Parser`:

```purescript
type Parser a
  = Input -> Either String a
```

For now, we will be using `String` to represent errors, but generally, you should be using a custom ADT instead.

1. Please rewrite `intParser` to follow the new `Parser` type. Check that these tests run correctly:

```purescript
intParserTest1 = assertEquals (intParser "port" exampleInput) (Right 8000)

intParserTest2 = assertEquals (intParser "port" $ Map.fromFoldable [ "pord" \/ [ "8000" ] ]) (Left "Missing option \"port\"")

intParserTest3 = assertEquals (intParser "host" exampleInput) (Left "Value of option \"host\" is not a valid integer")

intParserTest4 = assertEquals (intParser "port" $ Map.fromFoldable [ "port" /\ [ "8000", "8080" ] ]) (Left "Expected option \"port\" to have one value only")
```

2. Please rewrite `stringParser` to follow the new `Parser` type. Check that these tests run correctly:

```purescript
stringParserTest1 = assertEquals (stringParser "host" exampleInput) (Right "example.com")

stringParserTest2 = assertEquals (stringParser "whatever" exampleInput) (Left "Missing option \"whatever\"")

stringParserTest3 = assertEquals (stringParser "host" $ Map.fromFoldable [ "port" /\ [] ]) (Left "Option \"port\" has no value")

stringParserTest4 = assertEquals (stringParser "host" $ Map.fromFoldable [ "host" /\ [ "example.com", "purescript.org" ] ]) (Left "Expected option \"host\" to have one value only")
```

<!-- TODO: Add an exercise for a parser that only allows a string from an array. For example, when used for `scheme`, it should only allow `http` and `https` as values. -->

#### Pill 3

You might have noticed that we haven't defined what the types that represent our hypothetical command's options will look like. Since we are _totally_ good functional programmers here, we'll clearly model our problem domain now:

```purescript
data Scheme = Http | Https

derive instance eqScheme :: Eq Scheme

-- Server $SCHEME $HOST $PORT
data Server = Server Scheme String Int

derive instance eqServer :: Eq Server

type Credentials = { login :: String, password :: String }

data Options = Options Server Credentials

derive instance eqOptions :: Eq Options
```

Now that we have our desired types, we'll want to compose parsers so they would output them instead of unsafe strings.

1. Please write a function `schemeParser :: String -> Parser Scheme`. Make sure it passes the following tests:

```purescript
schemeParserTest1 = assertEquals (schemeParser "scheme" (Map.singleton "scheme" ["http"])) (Right Http)

schemeParserTest2 = assertEquals (schemeParser "scheme" (Map.singleton "scheme" ["https"])) (Right Https)

schemeParserTest3 = assertEquals (schemeParser "scheme" (Map.singleton "scheme" ["hddp"])) (Left "Expected option \"scheme\" to have value \"http\" or \"https\".")
```

2. Please write a function `serverParser :: Parser Scheme -> Parser String -> Parser Int -> Parser Server`. Make sure it passes the following tests:

```purescript
appliedServerParser = serverParser (schemeParser "scheme") (stringParser "host") (intParser "port")

serverParserTest1 = assertEquals (appliedServerParser $ Map.fromFoldable ["scheme" /\ [ "http" ], "host" /\ ["example.com"], "port" /\ ["8080"]]) (Right (Server Http "example.com" 8080))
```

3. Please write a function `credentialsParser :: Parser String -> Parser String -> Parser Credentials`. Make sure it passes the following tests:

```purescript
appliedCredentialsParser = credentialsParser (stringParser "login") (stringParser ("password"))

credentialsParserTest1 = assertEquals (appliedCredentialsParser $ Map.fromFoldable ["login" /\ ["admin"], "password" /\ ["123456"]]) (Right { login: "admin", password: "123456" })
```

4. Please write a function `optionsParser :: Parser Server -> Parser Credentials -> Parser Options`. Make sure it passes the following tests:
```purescript
appliedOptionsParser = optionsParser appliedServerParser appliedCredentialsParser

`ps-client --scheme https --host example.com --port 8000 --login admin --password hunter2`.

optionsParserTest = assertEquals (appliedOptionsParser exampleInput) (Right $ Options (Server Https "example.com" 8000) { login: "admin", password: "hunter2" })
```

### Pill 4

The obvious next step is to add help text to each option. To do that, we can change `Parser` into a `Tuple`:

```purescript
type Parser a
  = Tuple (Array String) (Input -> Either String a)
```

The idea here is that each parser will have its own help text, and when composing parsers, each parser's help text will be joined with the next one. At the end, we will be able to show the full command help text to the user.

<!-- TODO: add some exercises. -->
