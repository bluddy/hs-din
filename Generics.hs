{-# LANGUAGE DeriveGeneric, DefaultSignatures, FlexibleContexts, TypeOperators, FlexibleInstances #-}

import GHC.Generics
instance Serialize (Animation)

class Serialize a where
  put :: a -> String

  default put :: (Generic a, GSerialize (Rep a)) => a -> String
  put a = gput (from a)


-- Class to handle serialize generically
class GSerialize f where
  gput :: f a -> String

instance GSerialize U1 where
  gput U1 = []

instance GSerialize (K1 i c) where
  gput (K1 x) = []

-- For datatypes
instance (GSerialize a) => GSerialize (M1 D c a) where
  gput (M1 x) = "{" ++ gput x ++ "}"

-- For constructors
instance (GSerialize a, Constructor c) => GSerialize (M1 C c a) where
  gput m@(M1 x ) = conName m ++ " " ++ gput x

-- For record selectors
instance (GSerialize a) => GSerialize (M1 S c a) where
  gput (M1 x) = gput x

instance (GSerialize a, GSerialize b) => GSerialize (a :*: b) where
  gput (a :*: b) = gput a ++ gput b

instance (GSerialize a, GSerialize b) => GSerialize (a :+: b) where
  gput (L1 x) = gput x
  gput (R1 x) = gput x

