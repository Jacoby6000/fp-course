{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which. (It was Monad)

newtype Compose f g a =
  Compose (f (g a))

decompose :: Compose f g a -> f (g a)
decompose (Compose fga) = fga

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  (<$>) f (Compose fga) = Compose $ (f <$>) <$> fga

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure a = Compose (pure . pure $ a)
-- Implement the (<*>) function for an Applicative instance for Compose
  (<*>) (Compose fgf) (Compose fga) = Compose $ (<*>) <$> fgf <*> fga



-- instance (Monad f, Monad g) =>
  -- Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  -- (=<<) f (Compose fga) = Compose ((pure . decompose . f =<<) =<< fga)
