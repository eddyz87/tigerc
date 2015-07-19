module Main where

import Debug.Trace(traceIO)
import Lexer
import Parser
import Tokens
import Ast
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show $ parseString (foldr (++) "" args)
  return ()

parseString :: String -> Either String [Dec]
parseString str = 
  let tokens = runAlex str tokenize in
   case tokens of
   Right lst -> Right $ parse $ map token2TokenKind lst
   Left err -> Left $ "Lexer error: " ++ err


token2TokenKind :: Token -> TokenKind
token2TokenKind (Token _ k) = k

tokenize :: Alex [Token]
tokenize = loop []
  where
    loop acc = do
      t <- alexMonadScan
      case t of
       Token _ Eof -> return $ reverse acc
       _ -> loop $ t:acc
