module Types where

import Data.Map
import Ast (Id)

type Unique = Int

data Type =
    IntType
  | BoolType
  | StringType
  | Record [(Id, Type)] Unique
  | Array Type Unique
  | Nil
  | Unit
  | Syn Id
  deriving (Show, Eq)

data EnvEntry =
    VarEntry { varTyp :: Type }
  | FunEntry { fnFormals :: [Type],
               fnResult :: Type }
  deriving (Show, Eq)

type TypeEnv = Map Id Type
type VarEnv = Map Id EnvEntry
