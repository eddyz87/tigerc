module Tokens where

type Line = Int
type Column = Int

data Pos = Pos Line Column
         | NoPos
         deriving (Show)

data Token = Token { tokenPos :: Pos, tokenKind :: TokenKind }
             deriving (Show)

data TokenKind = Type
               | Var
               | Function
               | Break
               | Of
               | End
               | In
               | Nil
               | Let
               | Do
               | To
               | For
               | While
               | Else
               | Then
               | If
               | Array
               | Assign
               | Or
               | And
               | Ge
               | Gt
               | Le
               | Lt
               | Neq
               | Eq
               | Divide
               | Times
               | Minus
               | Plus
               | Dot
               | Rbrace
               | Lbrace
               | Rbrack
               | Lbrack
               | Rparen
               | Lparen
               | Semicolon
               | Colon
               | Comma
               | TString !String
               | TInt !Int 
               | Id !String
               | Eof
               deriving (Eq, Ord, Show)
                 
tokenId :: Token -> String
tokenId (Token _ (Id s)) = s
