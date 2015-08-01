module Main where

import Debug.Trace(traceIO)
import Lexer
import Parser
import Tokens
import Ast
import System.Environment(getArgs)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, liftM)
import Text.Show.Pretty (ppShow)
import Data.List
import Data.IORef

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show $ parseString (foldr (++) "" args)
  return ()

testParserInDir :: String -> IO ()
testParserInDir dir = do
  failed <- newIORef 0
  ok <- newIORef 0
  let testFile file = do
        r <- parseFile file
        case r of
         Left err -> do putStrLn $ file ++ ": " ++ err
                        modifyIORef failed (+1)
         _ -> modifyIORef ok (+1)
  files <- findFiles (isSuffixOf ".tig") dir
  mapM_ testFile files
  failed' <- readIORef failed
  ok' <- readIORef ok
  putStrLn $ "Fail: " ++ (show failed')
  putStrLn $ "Pass: " ++ (show ok')
  return ()

findFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
findFiles fn dir = do
  ents <- listDir dir
  (liftM concat) $ mapM getFiles ents
  where
    getFiles p = do
      isDir <- doesDirectoryExist p
      if isDir then
        findFiles fn p
      else return $ if (fn p) then [p] else []
    listDir dir = do
      ents <- getDirectoryContents dir
      return $ map ((</>) dir) $ sort $ filter notDots ents
    notDots p = p /= "." && p /= ".."

parseFile :: FilePath -> IO (Either String Exp)
parseFile file = readFile file >>= \s -> return $ parseString s

parseString :: String -> Either String Exp
parseString str = runAlex str tokenize >>= parse

tokenize :: Alex [Token]
tokenize = loop []
  where
    loop acc = do
      t <- alexMonadScan
      case t of
       Token _ Eof -> return $ reverse acc
       _ -> loop $ t:acc

