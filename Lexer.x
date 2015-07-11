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

tokens :-

<0> $white+            ;
<0> type               { tk Type }
<0> \"                 { \inp _ -> do
                                      startCode .= string
                                      stringStart .= alexPos inp
                                      alexMonadScan }
<0> "/*"               { st $ do
                                commentsLevel += 1
                                startCode .= comment
                                alexMonadScan
                       }
-- <string> \\\"          { stringchar }
<string> [^\"]         { \inp len -> do
                                       cur_str <- use curString
                                       curString .= ((alexMatch inp len) ++ cur_str)
                                       alexMonadScan }
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
<comment> .            ;

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
  inp <- alexGetInput
  returnToken inp Eof

tk tok = \inp _ -> returnToken inp tok
st fn  = \_ _ -> fn

alexMatch :: AlexInput -> Int -> String
alexMatch (_,_,_,str) len = take len str
alexPos (AlexPn _ l c,_,_,_) = Pos l c

returnToken inp tk = return $ Token (alexPos inp) tk
                               
alexMonadScan :: Alex Token
}
