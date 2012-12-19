module Animation where 

type FileName = String

data Index = Num Int
             | Entry
             | Exit
             deriving Show

data Tag = TagRightHand
        | TagLeftHand
        | TagChest
        | Origin
        | TagHelmet
        deriving Show

data Marker = SkillMarker
            | AttackMarker
            deriving Show

data Action = Sound FileName
            | StartEffect FileName Tag
            | StopEffect FileName Tag
            | SpawnEffect FileName Tag
            | StartClientModelSwipe FileName
            | StopClientModelSwipe FileName
            deriving Show
                

data AnimationMod = MatchMoveSpeed
                  | MatchAttackSpeed
                  | MatchSkillSpeed
                  deriving Show

data Animation = Animation {name::String,
                  file::FileName,
                  mod::Maybe AnimationMod,
                  client::Maybe [(Index, Marker)],
                  server::Maybe [(Index, Action)]
                 }
                 deriving Show


