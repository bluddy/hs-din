module Animation where 

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Perm
import Data.Functor ((<$>))
import Control.Applicative hiding ((<|>), many, optional)
import qualified Parser as P

type FileName = String

data Index = Num Int
             | Entry
             | Exit
             deriving Show

parseIndex :: Parser Index
parseIndex = string "entry" *> return Entry
               <|> string "exit" *> return Exit
               <|> (P.integer >>= \i -> return $ Num i)


data Tag = Origin
        | TagRightHand
        | TagLeftHand
        | TagChest
        | TagHelmet
        | TagRightFoot
        | TagLeftFoot
        deriving Show

parseTag :: Parser Tag
parseTag = choice [
            string "origin" *> return Origin,
            try(string "tag_righthand" *> return TagRightHand),
            try(string "tag_lefthand" *> return TagLeftHand),
            try(string "tag_rightfoot" *> return TagRightFoot),
            try(string "tag_leftfoot" *> return TagLeftFoot),
            try(string "tag_chest" *> return TagChest),
            try(string "tag_helmet" *> return TagHelmet)
           ]

data ServerAction = SkillMarker
            | AttackMarker
            deriving Show

parseServerAction :: Parser (Index, ServerAction)
parseServerAction = do
    index <- parseIndex <* P.s
    action <- choice [
        string "skillmarker" *> return SkillMarker,
        string "attackmarker" *> return AttackMarker 
       ]
    return (index, action)

data ClientAction = Sound FileName
            | StartEffect FileName Tag
            | StopEffect FileName Tag
            | SpawnEffect FileName Tag
            | StartClientModelSwipe FileName
            | StopClientModelSwipe FileName
            deriving Show

parseClientAction :: Parser (Index, ClientAction)
parseClientAction = do
    index <- parseIndex <* P.s
    action <- choice [
        try (hasPath "sound" Sound),
        try (hasTag "starteffect" StartEffect),
        try (hasTag "stopeffect" StopEffect),
        try (hasTag "spawneffect" SpawnEffect),
        try (hasPath "startclientmodelswipe" StartClientModelSwipe),
        try (hasPath "stopclientmodelswipe" StopClientModelSwipe)
       ]
    return (index, action)

        where hasTag name cons = do
                path <- string name *> P.s *> P.filePath <* P.s
                tag <- parseTag
                return $ cons path tag
              hasPath name cons = do 
                path <- string name *> P.s *> P.filePath
                return $ cons path

data AnimationMod = MatchMoveSpeed
                  | MatchAttackSpeed
                  | MatchSkillSpeed
                  deriving Show

parseMod :: Parser AnimationMod
parseMod = 
    choice [
        try(string "matchmovespeed" *> return MatchMoveSpeed),
        try(string "matchattackspeed" *> return MatchAttackSpeed),
        try(string "matchskillspeed" *> return MatchSkillSpeed)
       ]

data Animation = Animation {name::String,
                  file::FileName,
                  mod::Maybe AnimationMod,
                  client::[(Index, ClientAction)],
                  server::[(Index, ServerAction)]
                 }
                 deriving Show

parseServer :: Parser [(Index, ServerAction)]
parseServer = parseBlock "server" parseServerAction

parseClient :: Parser [(Index, ClientAction)]
parseClient = parseBlock "client" parseClientAction

parseBlock text parser = do 
    string text *> P.n
    char '{' *> P.n
    a <- many1 (parser <* P.n) 
    char '}'
    return a

parseAnimation :: Parser Animation
parseAnimation = do
    name <- P.name <* P.s
    file <- P.filePath <* P.n
    mod <- optionMaybe parseMod <* P.n
    char '{' *> P.n
    an <- permute $ Animation name file mod
            <$?> ([], parseClient <* P.n)
            <|?> ([], parseServer <* P.n)
    char '}' <* P.n
    return an





