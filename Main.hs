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

tokenize :: Alex [TokenKind]
tokenize = loop []
  where
    loop acc = do
      t <- alexMonadScan
      case t of
       Eof -> return $ reverse acc
       _ -> loop $ t:acc
