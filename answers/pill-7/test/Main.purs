module Test.Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Main (Compose(..))
import Test.Assert (assertEqual)

assertEqual' actual expected = assertEqual { actual, expected }

main :: Effect Unit
main = do
  let 
    pure' :: forall a. a -> Compose Array Maybe a
    pure' = pure
  log "Compose Functor instance"
  assertEqual' ((_ + 1) <$> pure' 5) (pure' 6)
  assertEqual' ((_ + 1) <$> Compose ([] :: _ (Maybe _))) (Compose [])
  assertEqual' ((_ + 1) <$> Compose [Nothing]) (Compose [Nothing])
  log "Compose Apply instance"
  assertEqual' ((+) <$> pure' 5 <*> pure' 6) (pure' 11)
  assertEqual' ((+) <$> pure' 6 <*> Compose []) (Compose [])
  assertEqual' ((+) <$> Compose [] <*> pure' 1) (Compose [])
  assertEqual' ((+) <$> Compose [Nothing] <*> pure' 1) (Compose [Nothing])
  log "Compose Applicative instance"
  assertEqual' (pure 5) (Compose [ Just 5 ])
  assertEqual' (pure 3) (Compose $ Just [ 3 ])
