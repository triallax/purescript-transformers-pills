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

We expect our minimum viable product (MVP) to be able to parse basic command-line options: `foo --bar 1 --baz 2 3 4 --bat string`.

Let's model our problem domain (I always wanted to say that):

```purescript
type Input = Map String (Array String)
```

`String` is the option name (`bar`, `baz`, with the double hypen removed), and `Array String` contains the values following the option.

The representation of the options in our command example would look like this:

```purescript
input1 = Map.fromFoldable [Tuple "foo" ["1"], Tuple "baz" ["2", "3", "4"], Tuple "bat" ["string"]]
```

We'll need to transform `Input` into well-typed values (`Int`, `String`, `Bool`, etc.). That's the purpose of `Parser`:

```purescript
type Parser a = Input -> Maybe a
```

Exercises:

- Please write a function `intParser :: String -> Parser Int`. `String` here represents the option name. Check that these tests run correctly:

```purescript
intParserTest1 = assertEquals (intParser "bar" input1) (Just 1)

intParserTest2 = assertEquals (intParser "baz" input1) Nothing

intParserTest3 = assertEquals (intParser "bat" input1) Nothing

intParserTest4 = assertEquals (intParser "x" input1) Nothing
```

- Please write a function `stringParser :: String -> Parser String`. Check that these tests run correctly:

```purescript
stringParserTest1 = assertEquals (stringParser "bar" input1) (Just "1")

stringParserTest2 = assertEquals (stringParser "bat" input1) (Just "string")

stringParserTest3 = assertEquals (stringParser "x" input1) Nothing

stringParserTest4 = assertEquals (stringParser "baz" input1) Nothing
```

- Please write a function `tupleParser :: forall a b. Parser a -> Parser b -> Parser (Tuple a b)`. Check that these tests run correctly:

<!-- TODO: provide two solutions: one by hand and one using Apply -->

```purescript
tupleParserTest1 = assertEquals (tupleParser (stringParser "bat") (intParser "bar")) (Just (Tuple "string" 1))

tupleParserTest2 = assertEquals (tupleParser (stringParser "x") (intParser "bat")) Nothing
```

- (Only for the curious since it is not directly relevant to our topic) Please write a function `fold :: Array String -> Maybe Input`. `fold` should transform from an array of command-line options to `Input`. Check that these tests run correctly:

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
