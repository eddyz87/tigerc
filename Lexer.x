{
module Lexer where

import Tokens
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

type AlexUserState = String
alexInitUserState = ""

mkStringAct :: AlexAction TokenKind
mkStringAct = \_ _ -> do
  str <- alexGetUserState
  return $ TString $ reverse str

accStringAct :: AlexAction TokenKind
accStringAct = \(_,_,_,inp) len -> do
                    str <- alexGetUserState
                    alexSetUserState $ (take len inp) ++ str
                    alexMonadScan
  
-- data State = MainState
--           | StringState string

}
