module Main where

import Debug.Trace(traceIO)
import Lexer
import Tokens

main :: IO ()
main = do
  traceIO $ show $ runAlex "type" tokenize
  return ()

tokenize :: Alex [TokenKind]
tokenize = loop []
  where
    loop acc = do
      t <- alexMonadScan
      case t of
       Eof -> return $ reverse acc
       _ -> loop $ t:acc
