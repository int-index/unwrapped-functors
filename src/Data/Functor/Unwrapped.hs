{-# LANGUAGE ExplicitNamespaces, TypeFamilies, UndecidableInstances #-}

module Data.Functor.Unwrapped
  ( Unwrappable(type Unwrapped, unwrap, wrap)
  ) where

import Data.Proxy
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Const
import Data.Functor.Compose
import Data.Functor.Sum

class Functor f => Unwrappable f where
  type Unwrapped f x
  unwrap :: f x -> Unwrapped f x
  wrap :: Unwrapped f x -> f x
  -- wrap . unwrap = id
  -- unwrap . wrap = id

instance Unwrappable Proxy where
  type Unwrapped Proxy x = ()
  unwrap Proxy = ()
  wrap () = Proxy

instance (Unwrappable f, Unwrappable g) => Unwrappable (Compose f g) where
  type Unwrapped (Compose f g) x = Unwrapped f (Unwrapped g x)
  unwrap (Compose a) = unwrap (fmap unwrap a)
  wrap a = Compose (fmap wrap (wrap a))

instance Unwrappable Identity where
  type Unwrapped Identity x = x
  unwrap (Identity x) = x
  wrap x = Identity x

instance Unwrappable (Const a) where
  type Unwrapped (Const a) x = a
  unwrap (Const a) = a
  wrap a = Const a

instance (Unwrappable f, Unwrappable g) => Unwrappable (Product f g) where
  type Unwrapped (Product f g) x = (Unwrapped f x, Unwrapped g x)
  unwrap (Pair a b) = (unwrap a, unwrap b)
  wrap (a, b) = Pair (wrap a) (wrap b)

instance (Unwrappable f, Unwrappable g) => Unwrappable (Sum f g) where
  type Unwrapped (Sum f g) x = Either (Unwrapped f x) (Unwrapped g x)
  unwrap (InL a) = Left (unwrap a)
  unwrap (InR b) = Right (unwrap b)
  wrap (Left a) = InL (wrap a)
  wrap (Right b) = InR (wrap b)
