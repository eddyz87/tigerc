module Ast where

type Id = String
type TypeId = Id

data Var = SimpleVar Id
         | FieldVar Var Id
         | SubscriptVar Var Exp
         deriving (Show)

type InitExp = Exp
type ToExp = Exp

data Exp = VarExp Var
         | NilExp
         | IntExp Int
         | StringExp String
         | CallExp Id [Exp]
         | OpExp Oper Exp Exp
         | RecordExp [(Id, Exp)] TypeId
         | SeqExp [Exp]
         | AssignExp Var Exp
         | IfExp Exp Exp (Maybe Exp)
         | WhileExp Exp Exp
         | ForExp Id InitExp ToExp Exp
         | BreakExp
         | LetExp [Dec] Exp
         | ArrayExp TypeId Exp InitExp
         deriving (Show)

data Dec = FunctionDec [FunDec]
         | VarDec Id (Maybe TypeId) Exp
         | TypeDec [(Id, Ty)]
         deriving (Show)

data Ty = NameTy Id
        | RecordTy [Field]
        | ArrayTy Id
        deriving (Show)

data FunDec = FunDec Id [Field] (Maybe TypeId) Exp
            deriving (Show)

data Field = Field Id TypeId
           deriving (Show)

data Oper = Plus
          | Minus
          | Mult
          | Div
          | Eq
          | Gt
          | Lt
          | Neq
          | Ge
          | Le
          | And
          | Or
          deriving (Show)
