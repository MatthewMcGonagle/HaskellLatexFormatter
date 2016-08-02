module LatexFormatter
( tokenize
, processIndent
, processIndentTokenlist
, Indent
, convertTokenlist
, findBegin
, findEq
, FindState(..)
) where

import Control.Monad.Writer
import Control.Monad.State

type Token = String
type Indent = Int

data Eqref = Eqref { origref :: String, eqnum :: Int}
data FindState = Normal | Begin | Equation | EqLabel | LabelName | End

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

processIndentToken :: Token -> State Indent Token
processIndentToken x = do
     current <- get
     let changeIndent = case x of 
          "\\begin" -> 1
          "\\end" -> -1 
          otherwise -> 0
     put $ current + changeIndent
     return x

processIndentTokenlist :: [Token] -> State Indent [Token]
processIndentTokenlist [] = return []
processIndentTokenlist x = do
      current <- get
      listToken <- mapM processIndentToken x
      newIndent <- get
      let newlistToken = case newIndent > current of
               True -> (replicate (4*current) ' '):x
               False -> (replicate (4*newIndent) ' '):x
      return newlistToken

processIndent :: [[Token]] -> State Indent [[Token]]
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

findEq :: Token -> State FindState Token
findEq x = do
     current <- get
     case current of
           Normal -> case x of
                 "\\begin" -> do 
                       put Begin
                       return x 
                 otherwise -> return x
           Begin -> case x of 
                 "{" -> return x 
                 "equation" -> do
                       put Equation
                       return "EQUATION"
                 otherwise -> do
                       put Normal
                       return x

           Equation -> case x of
                 "\\end" -> do
                        put End
                        return x 
                 "\\label" -> do 
                        put EqLabel
                        return x
                 otherwise -> return x

           EqLabel -> case x of
                 "{" -> do
                       put LabelName
                       return x
                 otherwise -> do
                       put Equation
                       return x

           LabelName -> do
                 put Equation
                 return $ "FOUND" ++ x

           End -> case x of
                "{" -> return x
                "equation" -> do
                      put Normal
                      return "EQUATION"
                otherwise -> do
                      put Equation
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
