{  
module Lexer where

import Tokens
import LexerState
import Control.Lens
}

%wrapper "monadUserState"

tokens :-

<0> $white+            ;
<0> type               { token $ \_ _ -> Type }
<0> \"                 { begin string }
-- <string> \\\"          { stringchar }
-- <string> [^\"]         { stringchar }
<string> [^\"]             { accStringAct }
<string> \"            { mkStringAct `andBegin` 0 }

{

alexEOF = return Eof

mkStringAct :: AlexAction TokenKind
mkStringAct = \_ _ -> do
  state <- alexGetUserState
  let str = view curString state
  return $ TString $ reverse str

accStringAct :: AlexAction TokenKind
accStringAct = \(_,_,_,inp) len -> do
                    state <- alexGetUserState
                    let str = view curString state
                    alexSetUserState $ set curString
                      ((take len inp) ++ str) state
                    alexMonadScan
  
-- data State = MainState
--           | StringState string

}
