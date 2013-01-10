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
parseIndex = try(P.strC "entry" *> return Entry)
               <|> try(P.strC "exit" *> return Exit)
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
            P.strC "origin" *> return Origin,
            try(P.strC "tag_righthand" *> return TagRightHand),
            try(P.strC "tag_lefthand" *> return TagLeftHand),
            try(P.strC "tag_rightfoot" *> return TagRightFoot),
            try(P.strC "tag_leftfoot" *> return TagLeftFoot),
            try(P.strC "tag_chest" *> return TagChest),
            try(P.strC "tag_helmet" *> return TagHelmet)
           ]

data ServerAction = SkillMarker
            | AttackMarker
            deriving Show

parseServerAction :: Parser (Index, ServerAction)
parseServerAction = do
    index <- parseIndex <* P.s
    action <- choice [
        P.strC "skillmarker" *> return SkillMarker,
        P.strC "attackmarker" *> return AttackMarker 
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
                path <- P.strC name *> P.s *> P.filePath <* P.s
                tag <- parseTag
                return $ cons path tag
              hasPath name cons = do 
                path <- P.strC name *> P.s *> P.filePath
                return $ cons path

data AnimationMod = MatchMoveSpeed
                  | MatchAttackSpeed
                  | MatchSkillSpeed
                  deriving Show

parseMod :: Parser AnimationMod
parseMod = 
    choice [
        try(P.strC "matchmovespeed" *> return MatchMoveSpeed),
        try(P.strC "matchattackspeed" *> return MatchAttackSpeed),
        try(P.strC "matchskillspeed" *> return MatchSkillSpeed)
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
    P.strC text *> P.n
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





