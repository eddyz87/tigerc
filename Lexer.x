{  
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lexer where

import Tokens
import LexerState
import Control.Lens
import Debug.Trace(trace)
import Control.Monad.State
import Data.Tuple (swap)

}

%wrapper "monadUserState"

tokens :-

<0> $white+            ;
<0> type               { tk Type }
<0> \"                 { begin string }
<0> "/*"               { modifyState1 (over commentsLevel (+1))
                           `andBegin` comment }
-- <string> \\\"          { stringchar }
<string> [^\"]         { accStringAct }
<string> \"            { mkStringAct `andBegin` 0 }

<comment> "/*"         { modifyState1 (over commentsLevel (+1)) }
<comment> "*/"         { \inp len -> do 
                                       -- state <- alexGetUserState
                                       userState . commentsLevel -= 1
                                       level <- use $ userState . commentsLevel
                                       if (level == 0)
                                       then do
                                         alexSetStartCode 0
                                         alexMonadScan
                                       else do
                                         alexMonadScan
                                    }
<comment> .            ;

{

instance MonadState AlexState Alex where
    state f = Alex $ \s -> Right $ swap $ f s
  
alexEOF = return Eof

tk tok = \_ _ -> return tok

alexCurrentMatch :: AlexInput -> Int -> String
alexCurrentMatch (_,_,_,str) len = take len str

mkStringAct :: AlexAction TokenKind
mkStringAct = \_ _ -> do
  state <- alexGetUserState
  let str = view curString state
  return $ TString $ reverse str

modifyState fn = \inp len ->
  do state <-alexGetUserState
     alexSetUserState $ fn inp len state
     alexMonadScan

modifyState1 fn = modifyState (\inp len -> fn)

accStringAct :: AlexAction TokenKind
accStringAct = modifyState (\inp len state ->
                    let str = view curString state
                        match = alexCurrentMatch inp len in
                     set curString (match ++ str) state)
  
-- data State = MainState
--           | StringState string

-- $(makeLensesBy (\n -> Just (n ++ "L")) ''AlexState)
userState :: Lens' AlexState AlexUserState
userState f alexState = fmap (\y -> alexState{ alex_ust=y }) (f (alex_ust alexState))

}
