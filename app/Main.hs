module Main where

import CommonParserUtil

import Lexer
import Parser
import Terminal

import Control.Monad (when)

import System.IO
import System.Environment (getArgs, withArgs)

import TokenInterface

import Text.Regex.TDFA

-- Syntax completion related
import EmacsServer
import SyntaxCompletion(computeCand)
import SyntaxCompletionSpec(spec)


-- Main
main :: IO ()
main = do
  args <- getArgs
  putStrLn "args:"
  mapM_ putStrLn args
  if "test" `elem` args
    then withArgs [] spec
    else if "emacs" `elem` args
      then emacsServer (computeCand False)
      else _main args

-- Todo: Can I fix to have "test" as a command in stack exec?

_main [] = return ()
_main (fileName:args) = 
   do _ <- doProcess True fileName
      _main args

doProcess verbose fileName = do
  text <- readFile fileName
  -- when (verbose) $ putStrLn "Lexing..."
  -- (_,_,terminalList) <- lexingWithLineColumn lexerSpec 1 1 text
  -- when (verbose) $ mapM_ putStrLn $ map terminalToString terminalList
  when (verbose) $ putStrLn "Parsing..."
  expr <- parsing False parserSpec ((),1,1,text)
  -- when (verbose) $ putStrLn (show expr)
  when (verbose) $ putStrLn "Done."

