{-# LANGUAGE DeriveGeneric, DefaultSignatures, FlexibleContexts, TypeOperators, FlexibleInstances #-}

module Animation where 
import GHC.Generics

type FileName = String

data Index = Num Int
             | Entry
             | Exit

data Tag = TagRightHand
        | TagLeftHand
        | TagChest
        | Origin
        | TagHelmet


data DebugTest = DebugOne
    deriving Generic

instance Serialize (DebugTest)

data Marker = SkillMarker
            | AttackMarker
            deriving Generic

instance Serialize (Marker)

data Action = Sound FileName
            | StartEffect FileName Tag
            | StopEffect FileName Tag
            | SpawnEffect FileName Tag
            | StartClientModelSwipe FileName
            | StopClientModelSwipe FileName
            deriving Generic

instance Serialize (Action)
                

data AnimationMod = MatchMoveSpeed
                  | MatchAttackSpeed
                  | MatchSkillSpeed
                  deriving Generic

instance Serialize (AnimationMod)

data Animation = Animation {name::String,
                  file::FileName,
                  mod::Maybe AnimationMod,
                  client::Maybe [(Index, Marker)],
                  server::Maybe [(Index, Action)]
                 }
                 deriving Generic

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

