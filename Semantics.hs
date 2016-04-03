{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Semantics where

import Ast
import Types
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad (foldM, unless)
import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Data.Maybe (isNothing, fromMaybe, fromJust, maybe)
import Main (parseString)
import Debug.Trace (trace, traceM)

type ErrMsg = String
type TypingResult = Either ErrMsg Type

baseTenv = M.fromList [("int", IntType)]

data TypeCheckerState = TypeCheckerState {
  _uidCounter :: Int,
  _tenv :: TypeEnv,
  _venv :: VarEnv
  }

makeLenses ''TypeCheckerState

initialTypeCheckerState =
  TypeCheckerState { _uidCounter = 0,
                     _tenv = baseTenv,
                     _venv = M.empty
                   }

type TypeChecked a = ExceptT String (State TypeCheckerState) a

runTypeChecked :: TypeCheckerState -> TypeChecked a -> Either String a
runTypeChecked state val = evalState (runExceptT val) state

nextUid :: TypeChecked Int
nextUid = do
  uid <- use uidCounter
  uidCounter += 1
  return uid

lookupEnv :: String -> (TypeCheckerState -> M.Map Id a)
          -> Id -> TypeChecked a
lookupEnv what accessor id = do
  env <- gets accessor
  case (M.lookup id env) of
    Just it -> return it
    Nothing -> throwError $ "Can't find " ++ what ++ " " ++ show id

forkEnv :: TypeChecked a -> TypeChecked a
forkEnv fn = do
  tenv' <- use tenv
  venv' <- use venv
  result <- fn
  tenv .= tenv'
  venv .= venv'
  return result

-- TODO: close Syn
lookupType :: Id -> TypeChecked Type
lookupType = lookupEnv "type" _tenv

lookupVar :: Id -> TypeChecked EnvEntry
lookupVar = lookupEnv "variable or function" _venv

insertEnv :: Id -> a -> Lens' TypeCheckerState (M.Map Id a)
          -> TypeChecked ()
insertEnv id val env = do
  curEnv <- use env
  env .= M.insert id val curEnv

addTypes :: [TyDec] -> TypeChecked ()
addTypes tys = do
  protos <- mapM addProto tys
  mapM_ checkProto protos
  where
    addProto (id, ty) = do
      proto <- mkProto ty
      insertEnv id proto tenv
      return (id, proto)
    mkProto (NameTy id) = return $ Syn id
    mkProto (ArrayTy id) = do
      uid <- nextUid
      return $ Array (Syn id) uid
    mkProto (RecordTy fields) = do
      uid <- nextUid
      let recFields = map (\(Field id tid) -> (id, Syn tid)) fields in
        return $ Record recFields uid
    checkProto (id, r) = do
      typ <- case r of
        Record fields uid -> do
          fields' <- mapM checkField fields
          return $ Record fields' uid
        Array typ uid -> do
          typ' <- closeSyn1 typ
          return $ Array typ' uid
        Syn _ -> closeSyn1 r
        _ -> return r
      insertEnv id typ tenv
    checkField (id, typ) = do
      typ' <- closeSyn1 typ
      return (id, typ')

closeSynAux fully = closeSyn' []
  where
    closeSyn' visited (Syn id) =
      if L.elem id visited
      then throwError $ "Type synonyms cycle: " ++ show visited
      else do
        typ <- lookupType id
        closeSyn' (id : visited) typ
    closeSyn' [] typ = return $ typ
    closeSyn' (id:_) typ =
      if fully
      then return typ
      else return $ Syn id

closeSyn = closeSynAux True
closeSyn1 = closeSynAux False

addFuns :: [FunDec] -> TypeChecked ()
addFuns funs = do
  spec <- mapM addFun funs
  mapM_ checkBody spec
  where
    addFun (FunDec id fields optTypId exp) = do
      fieldTyps <- mapM (lookupType . fieldTyp) fields
      funTyp <- case optTypId of
        Just typId -> lookupType typId
        Nothing -> return Unit
      insertEnv id (FunEntry fieldTyps funTyp) venv
      return $ (zip (map fieldName fields) fieldTyps, funTyp, exp)
    checkBody (formals, funTyp, body) = do
      forkEnv (do
                  mapM_ (uncurry addVar1) formals
                  assertType body funTyp)

addVar :: VarDec -> TypeChecked ()
addVar (VarDec id optTypId exp) = do
  expTyp <- expType exp
  typ <- case optTypId of
    Just typId -> lookupType typId
    Nothing -> expType exp
  assertSameTypes typ expTyp
  insertEnv id (VarEntry typ) venv

addVar1 :: Id -> Type -> TypeChecked ()
addVar1 id typ = insertEnv id (VarEntry typ) venv

addDec :: Dec -> TypeChecked ()
addDec dec =
  case dec of
    FunctionDec funs -> addFuns funs
    VariableDec var -> addVar var
    TypeDec tys -> addTypes tys

assertSameTypes :: Type -> Type -> TypeChecked ()
assertSameTypes expected actual =
  if expected == actual
    then return ()
    else throwError $
         "Expected type " ++ show expected ++
         " but got " ++ show actual

assertType :: Exp -> Type -> TypeChecked Type
assertType e typ = do
  et <- expType e
  assertSameTypes typ et
  return typ

expType :: Exp -> TypeChecked Type
expType e = case e of
  VarExp var -> varType var
  NilExp -> return Nil
  IntExp _ -> return IntType
  StringExp _ -> return StringType
  CallExp id actuals -> do
    ent <- lookupVar id
    case ent of
      FunEntry formalTyps typ -> do
        unless (length actuals == (length formalTyps))
          (throwError $ "Invalid number of arguments for " ++ show id)
        actyalTyps <- mapM expType actuals
        zipWithM_ assertSameTypes actyalTyps formalTyps
        return typ
      _ -> throwError $ "Expected " ++ show id ++ " to be a function"
  OpExp op l r ->
    case operKind op of
      IntToIntOper -> do
        assertType l IntType; assertType r IntType
      IntToBoolOper -> do
        assertType l IntType; assertType r IntType;
        return BoolType
      BoolToBoolOper -> do
        assertType l BoolType; assertType r BoolType
      AnyToBoolOper -> do
        lt <- expType l; assertType r lt;
        return BoolType
  RecordExp actuals typId -> do
    typ <- lookupType typId
    case typ of
      Record formals _ ->
        let formalNames = map fst actuals
            actualNames = map fst formals
            missingNames = formalNames L.\\ actualNames
            wrongNames = actualNames L.\\ formalNames
            sortedFormalTypes = map snd $ L.sortOn fst formals
            sortedActuals = map snd $ L.sortOn fst actuals
        in do
          unless (L.null missingNames)
            (throwError $
             "The following fields of "
             ++ show typId
             ++ " constructor are missing: "
             ++ show missingNames)
          unless (L.null wrongNames)
            (throwError $
             "The following fields "
             ++ show wrongNames
             ++ " are not defined for "
             ++ show typId)
          zipWithM_ assertType sortedActuals sortedFormalTypes
          return typ
      _ -> throwError $ "Expected " ++ show typId ++ " to be a record"
  SeqExp [] -> return Unit
  SeqExp exps -> do
    typs <- mapM expType exps
    return $ L.last typs
  AssignExp var exp -> do
    vart <- varType var
    assertType exp vart
    return Unit
  IfExp e1 e2 e3o -> do
    assertType e1 BoolType
    e2t <- expType e2
    e3t <- fromMaybe (return Unit) (fmap expType e3o)
    assertSameTypes e2t e3t
    return e2t
  WhileExp e1 e2 -> do
    assertType e1 BoolType
    expType e2
    return Unit
  BreakExp -> return Unit
  ArrayExp typId size init -> do
    assertType size IntType
    initTyp <- expType init
    typ <- lookupType typId
    case typ of
      Array arrTyp _ -> do
        assertSameTypes arrTyp initTyp
        return typ
      _ -> throwError $ "Expected " ++ show typId ++ " to be an array"
  ForExp id init to body -> do
    assertType to IntType
    forkEnv (do
                addVar $ VarDec id (Just "int") init
                expType body)           
    return Unit
  LetExp decs exp ->
    forkEnv (do
                mapM_ addDec decs
                expType exp)

varType :: Var -> TypeChecked Type
varType var =
  case var of
    SimpleVar id -> do
      ent <- lookupVar id
      case ent of
        VarEntry typ -> return typ
        _ -> throwError $ "Expected " ++ show id ++ " to be a variable"
    FieldVar var id -> do
      varTyp <- varType var
      case varTyp of
        Record fields _ ->
          case L.lookup id fields of
            Just typ -> return typ
            Nothing -> throwError $ "Can't find field " ++ show id
        _ -> throwError $
          "Expected a record near " ++ show id ++ " access"
    SubscriptVar var exp -> do
      varTyp <- varType var
      assertType exp IntType
      case varTyp of
        Array typ _ -> return typ
        _ -> throwError $
          "Expected " ++ show var ++ " to be an array"

data OperKind = IntToIntOper
              | IntToBoolOper
              | BoolToBoolOper
              | AnyToBoolOper
         
operKind op | L.elem op [Minus, Plus, Div, Mult] = IntToIntOper
            | L.elem op [Gt, Lt, Le, Ge] = IntToBoolOper
            | L.elem op [And, Or] = BoolToBoolOper
            | otherwise = AnyToBoolOper

                          
typeCheckString :: String -> IO ()
typeCheckString str =
    case typOrErr of
      Right typ -> putStrLn $ "Type is: " ++ show typ
      Left err -> putStrLn $ "Typing error detected: " ++ err
    where
      typOrErr = do
        exp <- parseString str
        runTypeChecked initialTypeCheckerState (expType exp)
