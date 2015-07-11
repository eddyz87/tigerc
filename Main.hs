module Main where

import Debug.Trace(traceIO)
import Lexer
import Tokens
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  traceIO $ show $ runAlex (foldr (++) "" args) tokenize
  return ()

tokenize :: Alex [Token]
tokenize = loop []
  where
    loop acc = do
      t <- alexMonadScan
      case t of
       Token _ Eof -> return $ reverse acc
       _ -> loop $ t:acc
