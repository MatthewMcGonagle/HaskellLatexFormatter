module LatexFormatter
( tokenline
, parsetokenline
, tokenize
, processIndent
, processIndentTokenlist
, Indent
, convertTokenlist
, findEq
, FindState(..)
, EqState(..)
, Token
) where

import Control.Monad.Writer
import Control.Monad.State
import Data.Monoid
import qualified Text.ParserCombinators.Parsec as P

type Token = String
data Textoken = Command String | Prose String | Symbol Char
     deriving (Show)
type Indent a = State Int a

data Eqref = Eqref { origref :: String, eqnum :: Int} 
     deriving (Show)
data FindState = Normal | Begin | Equation | EqLabel | LabelName | End 
     deriving (Eq)
data EqState = EqState {findstate :: FindState, eqcount :: Int}
type FindEq a = WriterT [Eqref] (State EqState) a

parsetokenline input = P.parse tokenline "An Error occurred." input
 
tokenline :: P.GenParser Char st [Textoken]
tokenline = (P.try P.spaces P.<|> return ()) >> P.endBy tokenword P.spaces

whitespace :: P.GenParser Char st () 
whitespace = P.skipMany1 $  P.char ' '

tokenword :: P.GenParser Char st Textoken
tokenword = P.try prose 
            P.<|> P.try slashprose
            P.<|> P.try texsymbol
            P.<|> P.try texcommand 
            P.<?> "Token Word" 

slashprose :: P.GenParser Char st Textoken
slashprose = do
                doubleslash <- P.lookAhead (P.string "\\\\")
                Prose following <- P.try prose
                return $ Prose ("\\\\" ++ following) 

texcommand :: P.GenParser Char st Textoken
texcommand = do
                slash <- P.char '\\'
                Prose name <- prose
                return $ Command name

texsymbol :: P.GenParser Char st Textoken
texsymbol = fmap (\ x -> Symbol x) $ P.oneOf "{}"

prose :: P.GenParser Char st Textoken
prose =  do
            letters <- P.many1 (P.noneOf nonlist)
            return $ Prose letters
    where nonlist = " \\{}"


-- Old Stuff

appendchar :: [String] -> Char -> [String]
appendchar [] x = case x of
    ' ' -> []
    '\t' -> []
    otherwise -> [[x]]
appendchar (y:ys) x = case x of
    ' ' -> case y of
         [] -> (y:ys)
         otherwise -> ([]:y:ys)
    '\t' -> case y of
         [] -> (y:ys)
         otherwise -> ([]:y:ys)
    '\\' -> (['\\']:y:ys)
    '{' -> ([]:['{']:y:ys)
    '}' -> ([]:['}']:y:ys)
    otherwise -> (x:y):ys

removeemptyToken :: [Token] -> [Token]
removeemptyToken = filter (/= [])

tokenize :: String -> [Token]
tokenize [] = []
tokenize x =  removeemptyToken . reverse . (map reverse) $ foldl
    appendchar
    []
    x

processIndentToken :: Token -> Indent Token
processIndentToken x = do
     current <- get
     let changeIndent = case x of 
          "\\begin" -> 1
          "\\end" -> -1 
          otherwise -> 0
     put $ current + changeIndent
     return x

processIndentTokenlist :: [Token] -> Indent [Token]
processIndentTokenlist [] = return []
processIndentTokenlist x = do
      current <- get
      listToken <- mapM processIndentToken x
      newIndent <- get
      let newlistToken = case newIndent > current of
               True -> (replicate (4*current) ' '):x
               False -> (replicate (4*newIndent) ' '):x
      return newlistToken

processIndent :: [[Token]] -> Indent [[Token]]
processIndent = mapM processIndentTokenlist

convertTokentoString :: Token -> String
convertTokentoString x  
    | x == [] = x 
    | 'a' < head x && head x < 'z' = ' ':x
    | 'A' < head x && head x < 'Z' = ' ':x
    | '0' < head x && head x < '9' = ' ':x
    | '}' == head x = ' ':x
    | '\\' == head x = ' ':x
    | '=' == head x = ' ':x
    | otherwise = x

convertTokenlist :: [Token] -> String
convertTokenlist x = foldr (\y acc -> (convertTokentoString y) ++ acc) "" x

testtokenstart :: Char -> Bool
testtokenstart x 
    | x == ' ' = False
    | otherwise = True

testtokenend :: Char -> Bool
testtokenend x
    | x == ' ' = True
    | x == '\\' = True
    | x == '{' = True
    | x == '}' = True
    | otherwise = False

updateFindState :: FindState -> Token -> FindState
updateFindState EqLabel "{" = LabelName
updateFindState y "{" = y

updateFindState Normal "\\begin" = Begin
updateFindState Begin "equation" = Equation
updateFindState Begin x = Normal
updateFindState Equation "\\end" = End
updateFindState Equation "\\label" = EqLabel 
updateFindState EqLabel x = Equation
updateFindState LabelName x = Equation
updateFindState End "equation" = Normal
updateFindState End x = Equation
updateFindState x y = x

findEq :: Token -> FindEq Token
findEq x = do
     current <- get
     let search = findstate current 
         count = eqcount current
         newsearch = updateFindState search x
         newcount = case search == Begin && newsearch == Equation of
               True -> count + 1
               False -> count
     case search of
          LabelName -> tell $ [Eqref x count] 
          otherwise -> tell []
     put $ EqState newsearch newcount
     return x

findEqref :: [Token] -> Writer [Eqref] [Token]
findEqref x = return x
