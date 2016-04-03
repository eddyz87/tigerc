module Ast where

type Id = String
type TypeId = Id

data Var = SimpleVar Id
         | FieldVar Var Id
         | SubscriptVar Var Exp
         deriving (Show)

type InitExp = Exp
type ToExp = Exp
type TrueExp = Exp
type FalseExp = Exp

data Exp = VarExp Var
         | NilExp
         | IntExp Int
         | StringExp String
         | CallExp Id [Exp]
         | OpExp Oper Exp Exp
         | RecordExp [(Id, Exp)] TypeId
         | SeqExp [Exp]
         | AssignExp Var Exp
         | IfExp Exp TrueExp (Maybe FalseExp)
         | WhileExp Exp Exp
         | ForExp Id InitExp ToExp Exp
         | BreakExp
         | LetExp [Dec] Exp
         | ArrayExp TypeId Exp InitExp
         deriving (Show)

data Dec = FunctionDec [FunDec]
         | VariableDec VarDec 
         | TypeDec [TyDec]
         deriving (Show)

type TyDec = (Id, Ty)

data VarDec = VarDec Id (Maybe TypeId) Exp 
            deriving (Show)

data FunDec = FunDec Id [Field] (Maybe TypeId) Exp
            deriving (Show)

data Ty = NameTy Id
        | RecordTy [Field]
        | ArrayTy Id
        deriving (Show)

data Field = Field { fieldName :: Id,  fieldTyp :: TypeId }
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
          deriving (Show, Eq)
