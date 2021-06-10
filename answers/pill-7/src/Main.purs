module Main where

import Prelude

import Data.Eq (class Eq1)

newtype Compose f g a
  = Compose (f (g a))

-- Just for testing.
derive instance (Eq1 f, Eq (g a)) => Eq (Compose f g a)

instance Show (f (g a)) => Show (Compose f g a) where
  show (Compose m) = "(Compose " <> show m <> ")"

instance functorCompose :: (Functor f, Functor g) => Functor (Compose f g) where
  map f (Compose fga) = Compose $ map (map f) fga

instance applyCompose :: (Apply f, Apply g) => Apply (Compose f g) where
  -- apply :: g (a -> b) -> g a -> g b
  -- apply :: f (g a -> g b) -> f (g a) -> f (g b)
  -- apply :: f (g a -> g b) -> f (g a) -> f (g b)
  -- apply :: f (g (a -> b)) -> f (g a) -> f (g b)
  apply (Compose fga2b) (Compose fga) = Compose (apply <$> fga2b <*> fga)

instance applicativeCompose :: (Applicative f, Applicative g) => Applicative (Compose f g) where
  -- pure :: a -> Compose f g a
  pure = Compose <<< pure <<< pure

-- | It's impossible to implement join for any two arbitrary monads.
-- | Berlin Functional Programming Group, Alejandro Serrano Mena:
-- | https://youtu.be/eZ9FpG8May8?t=2638
-- join :: f (g (f (g a))) -> f (g a)
-- joinCompose :: forall f g a. Compose f g (Compose f g a) -> Compose f g a
-- joinCompose (Compose fgComposefga) =
