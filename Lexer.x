{  
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lexer where

import Tokens
import LexerState
import Control.Lens
import Debug.Trace(trace)
import Control.Monad.State

}

%wrapper "monadUserState"

$digit = [0-9]
$letter = [a-zA-Z]

@idStart = $letter | _
@idBody  = @idStart | $digit

tokens :-

<0> $white+            ;
<0> "type"             { tk Type }
<0> "var"              { tk Var }  
<0> "function"         { tk Function }
<0> "break"            { tk Break }
<0> "of"               { tk Of }
<0> "end"              { tk End }
<0> "in"               { tk In }
<0> "nil"              { tk Nil }
<0> "let"              { tk Let }
<0> "do"               { tk Do }
<0> "to"               { tk To }
<0> "for"              { tk For }
<0> "while"            { tk While }
<0> "else"             { tk Else }
<0> "then"             { tk Then }
<0> "if"               { tk If }
<0> "array"            { tk Array }
<0> ":="               { tk Assign }
<0> "|"                { tk Or }
<0> "&"                { tk And }
<0> ">="               { tk Ge }
<0> ">"                { tk Gt }
<0> "<="               { tk Le }
<0> "<"                { tk Lt }
<0> "<>"               { tk Neq }
<0> "="                { tk Eq }
<0> "/"                { tk Divide }
<0> "*"                { tk Times }
<0> "-"                { tk Minus }
<0> "+"                { tk Plus }
<0> "."                { tk Dot }
<0> "}"                { tk Rbrace }
<0> "{"                { tk Lbrace }
<0> "]"                { tk Rbrack }
<0> "["                { tk Lbrack }
<0> ")"                { tk Rparen }
<0> "("                { tk Lparen }
<0> ";"                { tk Semicolon }
<0> ":"                { tk Colon }
<0> ","                { tk Comma }

<0> $digit+            { \inp len -> returnToken inp $ TInt $ read $ alexMatch inp len }
<0> @idStart @idBody*  { \inp len -> returnToken inp $ Id $ alexMatch inp len }

<0> \"                 { \inp _ -> do
                                      startCode .= string
                                      stringStart .= alexPos inp
                                      alexMonadScan }
<0> "/*"               { st $ do
                                commentsLevel += 1
                                startCode .= comment
                                alexMonadScan
                       }
<string> \\\"          { stringChar '"' }
<string> \\\\          { stringChar '\\' }
<string> \\n           { stringChar '\n' }
<string> [^\"]         { \inp len -> stringAccum (alexMatch inp len) }
<string> \"            { \inp _ -> do
                                     str <- use curString
                                     pos <- use stringStart
                                     curString .= ""
                                     stringStart .= nonePos
                                     startCode .= 0
                                     return $ Token pos (TString $ reverse str) }

<comment> "/*"         { st $ do
                                commentsLevel += 1
                                alexMonadScan }
<comment> "*/"         { st $ do 
                                commentsLevel -= 1
                                level <- use commentsLevel
                                if (level == 0)
                                  then do
                                    startCode .= 0
                                    alexMonadScan
                                  else alexMonadScan
                       }
<comment> .|\n         ;

{

instance MonadState AlexUserState Alex where
    state f = Alex $ \s ->
      let ust = alex_ust s
          (a, ust') = f ust
          s' = s { alex_ust = ust', alex_scd = _startCode ust' }
      in
       Right (s', a)

alexEOF :: Alex Token
alexEOF = do
  code <- use startCode
  case () of
   _ | code == comment -> alexError "Unmatched /* at the end of input"
     | code == string -> do
         pos <- use stringStart
         alexError $ "Unmatched \" at " ++ (show pos) ++ " at the end of input"
     | otherwise -> do
         inp <- alexGetInput
         returnToken inp Eof

tk tok = \inp _ -> returnToken inp tok
st fn  = \_ _ -> fn

alexMatch :: AlexInput -> Int -> String
alexMatch (_,_,_,str) len = take len str
alexPos (AlexPn _ l c,_,_,_) = Pos l c

returnToken inp tk = return $ Token (alexPos inp) tk

stringChar :: Char -> AlexAction Token
stringChar ch = \_ _ -> stringAccum [ch]

stringAccum :: String -> Alex Token
stringAccum str = do
  cur_str <- use curString
  curString .= (str ++ cur_str)
  alexMonadScan

  
alexMonadScan :: Alex Token
}
