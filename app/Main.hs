module Main where

import Lib
import Control.Monad.State
import LatexFormatter
import System.IO

main :: IO ()
main = do
    handle <- openFile "example.tex" ReadMode
    contents <- hGetContents handle
    let mystringlist = lines contents
        tokenlists = map tokenize mystringlist
    putStrLn "\nInput as a list of String:"
    sequence $ map putStrLn mystringlist

    let statelist = mapM processIndentTokenlist tokenlists 
        newtokenlists = fst $ runState statelist 0
        newstringlist = map convertTokenlist newtokenlists
    putStrLn "\nAs lists of Token after Indent processing:"
    sequence $ map print newtokenlists

    putStrLn "\nOutput as a list of String:"
    sequence $ map putStrLn newstringlist
    
    putStrLn "\nTest of finding equations:"
    let neweqlist = mapM (mapM findEq) newtokenlists
    sequence $ map print (fst $ runState neweqlist Normal)

    hClose handle
    return ()
