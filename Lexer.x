{  
module Lexer where

import Tokens
import LexerState
import Control.Lens
}

%wrapper "monadUserState"

tokens :-

<0> $white+            ;
<0> type               { tk Type }
<0> \"                 { begin string }
-- <string> \\\"          { stringchar }
-- <string> [^\"]         { stringchar }
<string> [^\"]         { accStringAct }
<string> \"            { mkStringAct `andBegin` 0 }

{

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

accStringAct :: AlexAction TokenKind
accStringAct = modifyState (\inp len state ->
                    let str = view curString state
                        match = alexCurrentMatch inp len in
                     set curString (match ++ str) state)
  
-- data State = MainState
--           | StringState string

}
