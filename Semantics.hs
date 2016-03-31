module Semantics where

import Ast
import Types
import qualified Data.Map as M
import qualified Data.List as L
import Control.Monad (foldM)
import Data.Maybe (isNothing, fromMaybe, fromJust, maybe)
import Main (parseString)
import Debug.Trace (trace, traceM)

type ErrMsg = String
type TypingResult = Either ErrMsg Type

baseTenv = M.fromList [("int", IntType)]
    
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
                    (True, True) ->
                        let formals' = L.sortOn fst formals
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
          do { vart <- tVar var; expectType exp vart; Right Unit }
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
            (tenv1, venv1) <- fillDecs tenv venv decs
            mapM_ (checkVarEnv tenv1 venv1) decs
            expType tenv1 venv1 exp
    checkVarEnv tenv venv (FunctionDec funs) =
        mapM_ checkFun funs
        where
          checkFun (FunDec id _ _ exp) = do
            ent <- venvLookup id venv
            expectType1 tenv venv exp (fnResult ent)
    checkVarEnv tenv venv (VariableDec (VarDec id _ exp)) = do
      ent <- venvLookup id venv
      expectType1 tenv venv exp (varTyp ent)
      return ()
    checkVarEnv _ _ (TypeDec _) = return ()
    venvLookup id venv =
        case M.lookup id venv of
          Just ent -> Right ent
          Nothing -> tErr $ "Can't find var entry for " ++ show id
    expectType1 :: TypeEnv -> VarEnv -> Exp -> Type -> TypingResult
    expectType1 tenv venv e typ = do
      et <- expType tenv venv e
      if et == typ
      then Right typ
      else Left $ "expected " ++ show e ++ " to be a/an " ++ show typ
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
    tErr msg = Left msg

qId id = "'" ++ id ++ "'"

data OperKind = IntToIntOper
              | IntToBoolOper
              | BoolToBoolOper
              | AnyToBoolOper
         
operKind op | L.elem op [Minus, Plus, Div, Mult] = IntToIntOper
            | L.elem op [Gt, Lt, Le, Ge] = IntToBoolOper
            | L.elem op [And, Or] = BoolToBoolOper
            | otherwise = AnyToBoolOper

fillDecs :: TypeEnv -> VarEnv -> [Dec] -> Either ErrMsg (TypeEnv, VarEnv)
fillDecs tenv venv decs = foldM fill (tenv, venv) decs
    where
      fill (tenv, venv) (FunctionDec funs) = do
        venv' <- fillFunDecs tenv venv funs
        Right (tenv, venv')
      fill (tenv, venv) (VariableDec var) = do
        venv' <- fillVarDec tenv venv var
        Right (tenv, venv')
      fill (tenv, venv) (TypeDec typs) = do
        tenv' <- fillTypeDecs tenv typs
        Right (tenv', venv)
                          
fillFunDecs :: TypeEnv -> VarEnv -> [FunDec] -> Either ErrMsg VarEnv
fillFunDecs tenv venv decs = foldM mkEntry venv decs
    where
      mkEntry acc (FunDec id fields optTypId _) = do
        fieldTyps <- mapM (lookupType tenv . fieldTyp) fields
        funTyp <- case optTypId of
                    Just typId -> lookupType tenv typId
                    Nothing -> Right Unit
        Right $ M.insert id (FunEntry fieldTyps funTyp) acc

fillVarDec :: TypeEnv -> VarEnv -> VarDec -> Either ErrMsg VarEnv
fillVarDec tenv venv dec = mkEntry dec
    where
      mkEntry (VarDec id optTypId exp) = do
        typ <- case optTypId of
                 Just typId -> lookupType tenv typId
                 Nothing -> expType tenv venv exp
        Right $ M.insert id (VarEntry typ) venv
                                        
fillTypeDecs :: TypeEnv -> [(Id, Ty)] -> Either ErrMsg TypeEnv
fillTypeDecs tenv pairs =
      foldM (\tenv id -> do
               typ <- lookupType tenv' id
               typ1 <- forceDelays typ
               return $ M.insert id typ1 tenv)
            tenv'
            (map fst pairs)
    where
      tenv' = L.foldl addProxy tenv pairs
      addProxy tenv (id, ty) = M.insert id (tyProxy ty) tenv
      tyProxy (NameTy id) = Syn id
      tyProxy (RecordTy fields) =
          Record (map (\(Field id tid) -> (id, Syn tid)) fields) 0
      tyProxy (ArrayTy id) = Array (Syn id) 0
      forceDelay' typ @ (Syn id) visited = do
          idTyp <- lookupType tenv' id
          if isSyn idTyp
          then if L.elem id visited
               then Left $ "Types cycle: " ++ show (L.reverse (id : visited))
               else forceDelay' idTyp $ id : visited
          else Right typ
      forceDelay' typ _ = Right typ
      forceDelay typ = forceDelay' typ []
      forceDelays (Record fields u) = do
        fields' <- mapM (\(id, typ) -> do
                           typ1 <- forceDelay typ
                           return (id, typ1))
                        fields
        return $ Record fields' u
      forceDelays (Array typ u) = do
        typ1 <- forceDelay typ
        return $ Array typ1 u
      forceDelays typ @ (Syn _) =
          forceDelay typ
      isSyn (Syn _) = True
      isSyn _ = False

lookupType :: TypeEnv -> Id -> TypingResult
lookupType tenv id =
    case M.lookup id tenv of
      Just typ -> Right typ
      Nothing -> Left $ "Can't find type named " ++ qId id
                          
typeCheckString :: String -> IO ()
typeCheckString str =
    case typOrErr of
      Right typ -> putStrLn $ "Type is: " ++ show typ
      Left err -> putStrLn $ "Typing error detected: " ++ err
    where
      typOrErr = parseString str >>= expType baseTenv M.empty
