module LatexFormatter
( tokenize
, processIndent
, processIndentTokenlist
, Indent
, convertTokenlist
) where

import Control.Monad.Writer
import Control.Monad.State

type Token = String
type Indent = Int

appendchar :: [String] -> Char -> [String]
appendchar [] x = case x of
    ' ' -> []
    otherwise -> [[x]]
appendchar (y:ys) x = case x of
    ' ' -> case y of
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
