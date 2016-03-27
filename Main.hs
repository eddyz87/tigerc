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
import Control.Monad.State
import Text.Show.Pretty (ppShow)
import Data.List
import Data.IORef
import qualified Data.List

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ show $ parseString (foldr (++) "" args)
  return ()

data ParserTestState =
  ParserTestState { stPassed :: Int, stErrors :: [(String, String)] }
type PS = StateT ParserTestState IO

testFile :: FilePath -> PS ()
testFile file = do
  r <- liftIO $ parseFile file
  case r of
   Left err -> modify $ \s -> s { stErrors = (file, err) : (stErrors s) }
   _ -> modify $ \s -> s { stPassed = stPassed s + 1 }
  return ()

testParserInDir :: String -> IO ()
testParserInDir dir = do
  files <- findFiles (isSuffixOf ".tig") dir
  st <- execStateT (sequence_ $ map testFile files) $ ParserTestState 0 []
  let sorted = sortBy (\x y -> compare (fst x) (fst y)) (stErrors st)
  forM_ sorted $ \(file, err) -> do
    putStrLn $ file ++ ": " ++ err
  putStrLn $ "Fail: " ++ (show $ length $ stErrors st)
  putStrLn $ "Pass: " ++ (show $ stPassed st)
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
      return $ map ((</>) dir) $ filter notDots ents
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
