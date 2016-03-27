module Semantics where

import Ast
import Types
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad (when)
import Data.Maybe (isNothing, fromMaybe)

type ErrorMsg = String
type TypingResult = Either [ErrorMsg] Type

expType :: TypeEnv -> VarEnv -> Exp -> TypingResult
expType tenv venv = tExp
  where
    tExp e = case e of
      VarExp var -> tVar var
      NilExp -> Right Nil
      IntExp _ -> Right IntType
      StringExp _ -> Right StringType
      CallExp id actuals -> case M.lookup id venv of
        Just (FunEntry formalTyps typ) -> 
          if length actuals /= (length formalTyps) then
             tErr $ "wrong number of actuals passed to " ++ show id
          else do
            actualTyps <- mapM tExp actuals
            case L.find (\(a, b, _) -> a /= b) $ zip3 actualTyps formalTyps actuals of
              Just (actyalTyp, _, actual) ->
                tErr $ "expected " ++ show actual ++ " to be of type " ++ show actyalTyp
              Nothing -> Right typ
        _ -> tErr $ show id ++ " is not a function"
      OpExp op l r ->
          case operKind op of
            IntOper -> do { expectType l IntType; expectType r IntType }
            BoolOper -> do { expectType l BoolType; expectType r BoolType }
            AnyToBoolOper -> do { lt <- tExp l; expectType r lt }
      RecordExp actuals recTypId ->
          case M.lookup recTypId tenv of
            Just recTyp @ (Record formals _) ->
                let formalNames = map fst actuals
                    actualNames = map fst formals
                    missingNames = formalNames L.\\ actualNames
                    wrongNames = actualNames L.\\ formalNames
                in
                  case (L.null missingNames, L.null wrongNames) of
                    (True, True) -> let formals' = L.sortOn fst formals
                                        actuals' = L.sortOn fst actuals
                                    in do
                                      mapM_ (\((name, typ), (_, a)) -> do
                                               at <- tExp a
                                               if at == typ
                                               then Right typ
                                               else tErr $ "wrong type of field " ++ qId name ++ " initializer" ++
                                                           " should be " ++ show typ ++ " but found " ++ show at)
                                            (L.zip formals' actuals')
                                      Right recTyp
                    (False, _) -> tErr $ "the field names are missing: " ++ show missingNames
                    (_, False) ->
                        tErr $ "the field names are not in record " ++ qId recTypId ++ ": " ++ show missingNames
            Just _ -> tErr $ show recTypId ++ " should be a record type name"
            Nothing -> tErr $ "can't find a type named " ++ show recTypId
      SeqExp [] -> Right Unit
      SeqExp el -> do { elt <- mapM tExp el; Right $ L.last elt }
      AssignExp var exp ->
          do { vart <- tVar var; expectType exp vart }
      IfExp e1 e2 e3_opt ->
          do
            expectType e1 BoolType
            e2_t <- tExp e2
            case e3_opt of
              Just e3 -> expectType e3 e2_t
              Nothing -> Right Unit
            Right Unit
      WhileExp e1 e2 ->
          do
            expectType e1 BoolType
            tExp e2
            Right Unit
      BreakExp -> Right Unit
      ArrayExp typ sizeExp initExp -> undefined
      ForExp id initExp toExp exp ->
          do
            expectType initExp IntType
            expectType toExp IntType
            expType tenv (M.insert id (VarEntry IntType) venv) exp
            Right Unit
      LetExp decs exp ->
          let (tenv', venv') = addDecs decs in
          do
            mapM_ tDec decs
            expType tenv' venv' exp
      _ -> undefined
    addDecs decs = undefined
    addDec (tenv, venv) dec =
        case dec of
          FunDec id formals typId_opt exp ->
              let formalTyps = map (lookupType . snd) formals
                  typ = case typId_opt of
                          -- this shows only the types seen so far
                          Just typId -> lookupType tenv typId
                          Nothing -> (expType tenv venv exp)
                  entry = FunEntry formalTyps typ
              in (tenv, M.insert id entry venv)
    lookupType tenv id = M.lookup id tenv
    tDec = undefined
    expectType e typ = do
      et <- tExp e
      if et == typ
      then Right $ typ
      else tErr $ "expected " ++ show e ++ " to be a/an" ++ (show typ)
    tVar v = case v of
      SimpleVar id -> tId id
      FieldVar var id -> do
        varT <- tVar var
        case varT of
          Record fields _ -> case L.lookup id fields of
            Just typ -> Right typ
            Nothing -> tErr $ "can't find record field " ++ qId id ++ " in " ++ show varT
          _ -> tErr $ show var ++ " is not a record, can't subscript"
      SubscriptVar var exp -> do
        varT <- tVar var
        case varT of
          Array typ _ -> Right typ
          _ -> tErr $ show var ++ " is not an array, can't subscript it"
    tId id = case M.lookup id venv of
      Just (VarEntry tp) -> Right tp
      Just _ -> tErr $ qId id ++ " is a function but used as a varaible"
      Nothing -> tErr $ "can't find a variable named " ++ qId id
    tErr msg = Left [msg]

qId id = "'" ++ id ++ "'"

data OperKind = IntOper | BoolOper | AnyToBoolOper
         
operKind op | L.elem op [Minus, Plus, Div, Mult, Gt, Lt, Le, Ge] = IntOper
            | L.elem op [And, Or] = BoolOper
            | True = AnyToBoolOper
         
-- data ExpTy = ExpTy Type

-- transVar :: VarEnv -> TypeEnv -> Var -> ExpTy
-- transExp :: VarEnv -> TypeEnv -> Exp -> ExpTy
-- transDec :: VarEnv -> TypeEnv -> Dec -> (VarEnv, TypeEnv)
-- transTy  :: TypeEnv -> Ty -> Type

-- transVar = undefined
-- transExp = undefined
-- transDec = undefined
-- transTy = undefined
