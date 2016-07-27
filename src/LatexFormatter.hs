module LatexFormatter
( tokenize
, processIndent
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

tokentoIndent :: Token -> Indent
tokentoIndent x 
     | x == "\\begin" = 1
     | x == "\\end" = -1
     | otherwise = 0
 
findIndentchange :: [Token] -> Indent
findIndentchange [] = 0
findIndentchange x = foldl (\acc y -> acc + (tokentoIndent y)) 0 x

processIndent :: [[Token]] -> State Indent [[Token]]
processIndent [] = return []
processIndent x = do
                      wrongorder <- foldM accIndentmap [] x
                      return $ reverse wrongorder 
     where accIndentmap :: [[Token]] -> [Token] -> State Indent [[Token]]
           accIndentmap acc y = do
                currentIndent <- get
                let newIndent = currentIndent + (findIndentchange y) 
                    newTokenlist = (replicate (4*newIndent) ' '):y
                put newIndent
                return (newTokenlist:acc)

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
