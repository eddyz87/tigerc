module Tokens where

data Pos = Pos Int Int
data Token = Token Pos TokenKind
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
               | TString String
               | TInt Int 
               | Id
               | Eof
               deriving (Eq, Ord, Show)
                 
