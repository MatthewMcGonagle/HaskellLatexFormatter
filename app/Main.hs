module Main where

import Lib
import Control.Monad.State
import Control.Monad.Writer
import LatexFormatter
import System.IO
import qualified Text.ParserCombinators.Parsec as Parsec

main :: IO ()
main = do
    handle <- openFile "example.tex" ReadMode
    contents <- hGetContents handle
    let mystringlist = lines contents
        tokenlists = map tokenize mystringlist
        tokenlists2 = map parsetokenline  mystringlist
    putStrLn "\nInput as a list of String:"
    sequence $ map putStrLn mystringlist

    putStrLn "\nToken Lists generated using Parsec:"
    let errormap :: Either Parsec.ParseError [a] -> [a]
        errormap (Right x) = x
        errormap (Left x) = []
    sequence $ map print (map errormap tokenlists2)

    let statelist = mapM processIndentTokenlist tokenlists 
        newtokenlists = fst $ runState statelist 0
        newstringlist = map convertTokenlist newtokenlists
    putStrLn "\nAs lists of Token after Indent processing:"
    sequence $ map print newtokenlists

    putStrLn "\nOutput as a list of String:"
    sequence $ map putStrLn newstringlist
    
    putStrLn "\nTest of finding equations:"
    let neweqlist = mapM (mapM findEq) newtokenlists
        initstate = EqState Normal 0
        witheqcount = runState (runWriterT neweqlist) initstate 
    putStrLn "\n The equation count: "
    print . eqcount . snd $ witheqcount
    putStrLn "\n The equation reference list: "
    sequence . (map print) . snd. fst  $ witheqcount

    hClose handle
    return ()
