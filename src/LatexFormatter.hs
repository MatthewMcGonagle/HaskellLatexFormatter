module LatexFormatter
( tokenize
, processIndent
, processIndentTokenlist
, Indent
, convertTokenlist
, findBegin
, findEq
, FindState(..)
, EqState(..)
) where

import Control.Monad.Writer
import Control.Monad.State
import Data.Monoid

type Token = String
type Indent a = State Int a

data Eqref = Eqref { origref :: String, eqnum :: Int} deriving (Show)
data FindState = Normal | Begin | Equation | EqLabel | LabelName | End
data EqState = EqState {findstate :: FindState, eqcount :: Int}
type FindEq a = WriterT [Eqref] (State EqState) a

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

findEq :: Token -> FindEq Token
findEq x = do
     current <- get
     let search = findstate current
         count = eqcount current
     case search of
           Normal -> case x of
                 "\\begin" -> do 
                       put $ EqState Begin count
                       return x 
                 otherwise -> return x
           Begin -> case x of 
                 "{" -> return x 
                 "equation" -> do
                       put $ EqState Equation (count+1)
                       return "EQUATION"
                 otherwise -> do
                       put $ EqState Normal count
                       return x

           Equation -> case x of
                 "\\end" -> do
                        put $ EqState End count
                        return x 
                 "\\label" -> do 
                        put $ EqState EqLabel count
                        return x
                 otherwise -> return x

           EqLabel -> case x of
                 "{" -> do
                       put $ EqState LabelName count
                       return x
                 otherwise -> do
                       put $ EqState Equation count
                       return x

           LabelName -> do
                 put $ EqState Equation count
                 tell $ [Eqref x count]
                 return x

           End -> case x of
                "{" -> return x
                "equation" -> do
                      put $ EqState Normal count
                      return "EQUATION"
                otherwise -> do
                      put $ EqState Equation count
                      return x

findBegin :: Token -> State Bool [Token]
findBegin x = do
     current <- get
     case current of
          True -> return [x]
          False -> case x of
               "\\begin" -> do
                      put True
                      return [x]
               otherwise -> return []

findEqref :: [Token] -> Writer [Eqref] [Token]
findEqref x = return x
