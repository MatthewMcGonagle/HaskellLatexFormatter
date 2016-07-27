module Main where

import Lib
import Control.Monad.State
import LatexFormatter

main :: IO ()
main = do
    let mystringlist = [ "     " ++ ['\\'] ++ "documentclass{article}"
                       , "  \\begin{document}"
                       , "  Hello World!!!!"
                       , "  \\end{document}"
                       ]
        tokenlists = map tokenize mystringlist
    putStrLn "\nInitial Input String List:"
    sequence $ map putStrLn mystringlist

    putStrLn "\nAs a list of Token:"
    sequence $ map print tokenlists
    let statelist = processIndent tokenlists 
        newtokenlists = fst $ runState statelist 0
        newstringlist = map convertTokenlist newtokenlists
    putStrLn "\nAfter Indent Processing:"
    sequence $ map print newtokenlists
    putStrLn "\nAs a list of String:"
    sequence $ map putStrLn newstringlist
    return ()