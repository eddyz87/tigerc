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
<0> \"                 { st $ do startCode .= string; alexMonadScan }
<0> "/*"               { st $ do
                                commentsLevel += 1
                                startCode .= comment
                                alexMonadScan
                       }
-- <string> \\\"          { stringchar }
<string> [^\"]         { mt $ \str -> do
                                        cur_str <- use curString
                                        state <- get
                                        curString .= (str ++ cur_str)
                                        alexMonadScan }
<string> \"            { st $ do
                                cur_str <- use curString
                                curString .= ""
                                startCode .= 0
                                return $ TString $ reverse cur_str }

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

alexEOF = return Eof

tk tok = \_ _ -> return tok
st fn = \_ _ -> fn
mt fn = \inp len -> fn $ alexCurrentMatch inp len

alexCurrentMatch :: AlexInput -> Int -> String
alexCurrentMatch (_,_,_,str) len = take len str


}
