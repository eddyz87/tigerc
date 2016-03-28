module Semantics where

import Ast
import Types
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad (foldM)
import Data.Maybe (isNothing, fromMaybe, fromJust)
import Main (parseString)

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
            IntToIntOper -> do { expectType l IntType; expectType r IntType }
            IntToBoolOper -> do { expectType l IntType; expectType r IntType; Right BoolType }
            BoolToBoolOper -> do { expectType l BoolType; expectType r BoolType }
            AnyToBoolOper -> do { lt <- tExp l; expectType r lt; Right BoolType }
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
          do
            (tenv', venv') <- addDecs decs
            mapM_ (tDec tenv' venv') decs
            expType tenv' venv' exp
    addDecs decs = foldM mkDec (tenv, venv) decs
    mkDec :: (TypeEnv, VarEnv) -> Dec -> Either [String] (TypeEnv, VarEnv)
    mkDec (tenv, venv) dec =
        case dec of
          FunDec id formals optTypId exp ->
              do
                formalTyps <- mapM (lookupType tenv . fieldTyp) formals
                funTyp <- case optTypId of
                            Just id -> lookupType tenv id
                            Nothing -> Right Unit
                let entry = FunEntry formalTyps funTyp in
                  Right (tenv, M.insert id entry venv)
          VarDec id optTypId exp ->
              do
                varTyp <- case optTypId of
                            Just typId -> do
                              lookupType tenv typId
                              --expectType exp typ
                            Nothing -> expType tenv venv exp
                let entry = VarEntry varTyp in
                  Right (tenv, M.insert id entry venv)
          TypeDec id ty ->
              do
                typ <- case ty of
                         NameTy id ->
                             lookupType tenv id >>= Right . Syn
                         RecordTy fields -> undefined
                         ArrayTy id -> undefined
                Right (M.insert id typ tenv, venv)
    lookupType tenv id =
        case M.lookup id tenv of
          Just typ -> Right typ
          Nothing -> tErr $ "Can't find type named " ++ qId id
    tDec tenv venv dec =
        case dec of
          -- TODO: fromJust is a bloody hack here
          FunDec id _ _ exp ->
              expectType1 tenv venv exp (fnResult $ fromJust $ M.lookup id venv)
          VarDec id (Just typId) exp ->
              expectType1 tenv venv exp (varTyp . fromJust $ M.lookup id venv)
          VarDec id _ _ -> Right (varTyp . fromJust $ M.lookup id venv)
          TypeDec _ _ -> Right Unit
    expectType1 tenv venv e typ = do
      et <- expType tenv venv e
      if et == typ
      then Right typ
      else tErr $ "expected " ++ show e ++ " to be a/an" ++ show typ
    expectType = expectType1 tenv venv
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

data OperKind = IntToIntOper
              | IntToBoolOper
              | BoolToBoolOper
              | AnyToBoolOper
         
operKind op | L.elem op [Minus, Plus, Div, Mult] = IntToIntOper
            | L.elem op [Gt, Lt, Le, Ge] = IntToBoolOper
            | L.elem op [And, Or] = BoolToBoolOper
            | otherwise = AnyToBoolOper
         

typeCheckString :: String -> Either String Types.Type
typeCheckString str =
  case parseString str of
    Right ast ->
      case expType M.empty M.empty ast of
        Right typ -> Right typ
        Left errs -> Left $ "Typing errors detected:\n" ++ (L.intercalate "\n" errs)
    Left err -> Left err

