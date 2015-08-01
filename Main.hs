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
parseString str = do
  tokens <- runAlex str tokenize
  parse $ map token2TokenKind tokens

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

-- testParserInDir :: String -> IO ()
-- testParserInDir dir = do
--   ;;
--   return ()
