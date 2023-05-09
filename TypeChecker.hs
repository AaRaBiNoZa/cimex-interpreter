module TypeChecker where

import qualified Data.Map as M
import Data.Map(Map)

import qualified Data.Set as S
import Data.Set(Set)

import qualified Cimex.Abs as C
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import Cimex.Abs (BNFC'Position)

-- not importing from Abs, since I don't need position information
data Type
    = IntT
    | StrT
    | BoolT
    | ArrT Type
    | FunT Type [(Type, Bool)] -- arg type and isref
  deriving (Eq, Ord, Show)

data TCEnv = TCEnv {
    types :: Map C.Ident Type,
    blockVars :: Set C.Ident,
    insideLoop :: Bool,
    insideFunc :: Maybe Type
} deriving Show

data Error = Error {desc :: String, location :: C.BNFC'Position} deriving Show

type TCMonad a = (ReaderT TCEnv (ExceptT Error Identity)) a

-- UTILITY
emptyEnv :: TCEnv
emptyEnv = TCEnv {types = M.empty, blockVars = S.empty, insideLoop = False, insideFunc = Nothing}

showPos :: BNFC'Position -> String
showPos (Just (line, col)) = show line ++ ":" ++ show col
showPos Nothing = "??:??"

showErr :: Error -> String
showErr (Error desc loc) = desc ++ "\n" ++ showPos loc

showTypeOptions :: Show a => Set a -> String
showTypeOptions set = go (S.toList set) where
    go [] = ""
    go [x] = show x
    go (x:xs) = show x ++ " | " ++ go xs

parseType :: C.Type -> Type
parseType (C.Int pos) = IntT
parseType  (C.Str pos) = StrT
parseType (C.Bool pos) = BoolT
parseType (C.Arr pos t) = ArrT (parseType t)


checkTypeIsIn :: Type -> Set Type -> C.BNFC'Position -> String -> TCMonad ()
checkTypeIsIn t good pos msg = do
    if S.member t good then
        return ()
    else
        throwError $ Error ("Type mismatch in " ++ msg ++ ": \nExpected types: " ++ showTypeOptions good ++ "\nGot: " ++ show t) pos

checkTypeIs :: Type -> Type -> C.BNFC'Position ->  String -> TCMonad ()
checkTypeIs t1 t2 = checkTypeIsIn t1 (S.singleton t2)

-- gets size  of dims
-- it is called only for array types
getArrayDimSize :: Type -> C.BNFC'Position -> TCMonad Int
getArrayDimSize arr@(ArrT t) pos = go arr pos where
    go :: Type -> C.BNFC'Position -> TCMonad Int
    go tp position = case tp of
        FunT _ _ -> throwError $ Error "Impossible - invalid array type: functions cannot be elements of an array" pos
        ArrT tp2 -> do
            rest  <- go tp2 position
            return $ rest + 1
        _basicType -> return 0
getArrayDimSize _notArray pos = throwError $ Error "Impossible - expected array" pos

isArray :: Type -> Bool
isArray (ArrT _) = True
isArray _ = False

-- creates a type of n-dimensional array of type t
getNDimArrayT :: Type -> Int -> Type
getNDimArrayT t 0 = t
getNDimArrayT t n = ArrT (getNDimArrayT t (n-1))

-- gets base type from n-dimensional array
getArrayBaseT :: Type -> Type
getArrayBaseT (ArrT t) = getArrayBaseT t
getArrayBaseT t = t

-- checks if indices are IntT and gets number of dimensions
getArrayIndicesSize :: [C.ArrArg] -> String -> TCMonad Int
getArrayIndicesSize [] msg = return 0
getArrayIndicesSize ((C.ArrIdx pos e):xs) msg = do
    tp <- typeOf e
    checkTypeIs tp IntT pos msg
    rest <- getArrayIndicesSize xs msg
    return $ rest + 1

-- TYPECHECKER

typeOf :: C.Expr -> TCMonad Type
typeOf (C.ELitTrue pos) = return BoolT
typeOf (C.ELitFalse pos) = return BoolT
typeOf (C.ELitInt pos _) = return IntT
typeOf (C.EString pos _) = return StrT
typeOf (C.EVar pos v) = do
    typesEnv <- asks types
    case M.lookup v typesEnv of
        Nothing -> throwError $ Error "Usage of undeclared variable" pos
        Just t -> return t

typeOf (C.Neg pos e) = do
    et <- typeOf e
    checkTypeIs et IntT pos "Negation"
    return et

typeOf (C.Not pos e) = do
    et <- typeOf e
    checkTypeIs et BoolT pos "Not"
    return et

typeOf (C.ECrtArr pos t@(C.Arr _ _) dims) = do
    let tp = parseType t
    typeDims <- getArrayDimSize tp pos
    indicesDims <- getArrayIndicesSize dims "Create array"
    if typeDims == indicesDims then
        return tp
    else
        throwError $ Error "Invalid dimensions when creating an array" pos

typeOf (C.ECrtArr pos _ _) = throwError $ Error "Expected an array in create array" pos

typeOf (C.EApp pos ident args) = do
    fType <- asks (M.lookup ident . types)
    case fType of
        Just (FunT retType params) -> do
            argsSigs <- mapM getSignature args
            assertArgsMatchParams argsSigs params pos
            return retType
        Just _ -> throwError $ Error "Cannot perform function application on not function type" pos
        Nothing -> throwError $ Error ("Function with identifier " ++ show ident ++ " doesn't exist") pos
    where
        getSignature :: C.Expr -> TCMonad (Type, Bool, C.BNFC'Position)
        getSignature arg = do
            t <- typeOf arg
            let canBeRef = case arg of {C.EVar pos name -> True; _ -> False}
            return (t, canBeRef, pos)

        assertArgsMatchParams :: [(Type, Bool, C.BNFC'Position)] -> [(Type, Bool)] -> C.BNFC'Position -> TCMonad ()
        assertArgsMatchParams [] [] appPos = return ()
        assertArgsMatchParams argsSigs [] appPos = throwError $ Error "Too many arguments in function application" appPos
        assertArgsMatchParams [] params appPos = throwError $ Error "Not enough arguments in function application" appPos
        assertArgsMatchParams ((t1, canRef, argPos):r1) ((t2, shouldBeRef):r2) appPos
          | t1 /= t2 =
            throwError $ Error "Argument type does not match parameter type at: " argPos
          | not canRef && shouldBeRef =
            throwError $ Error "Argument needs to be a variable (reference error)" argPos
          | otherwise =
            assertArgsMatchParams r1 r2 appPos

typeOf (C.EArrGet pos ident idxs) = do
    tp <- asks (M.lookup ident . types)
    case tp of
        Nothing -> throwError $ Error "Undeclared array" pos
        Just arr@(ArrT _) -> do
            typeDims <- getArrayDimSize arr pos
            indicesDims <- getArrayIndicesSize idxs "Array access"
            if typeDims >= indicesDims then
                return $ getNDimArrayT (getArrayBaseT arr) (typeDims - indicesDims)
            else
                throwError $ Error "Too many indices for this array" pos
        _notArray -> throwError $ Error ("Variable : " ++ show ident ++ " is not an array") pos 

typeOf (C.EAdd pos e1 op e2) = do
    t1 <- typeOf e1
    checkTypeIs t1 IntT pos "Add operation"
    t2 <- typeOf e2
    checkTypeIs t2 IntT pos "Add operation"
    return IntT

typeOf (C.EMul pos e1 op e2) = typeOf (C.EAdd pos e1 (C.Plus Nothing) e2)

typeOf (C.ERel pos e1 (C.EQU relpos) e2) = do
    t1 <- typeOf e1
    t2 <- typeOf e2
    if t1 == t2 then
        return BoolT
    else
        throwError $ Error "Comparison on two different types " pos

typeOf (C.ERel pos eq (C.NE relpos) e2) = typeOf (C.ERel pos eq (C.EQU relpos) e2)

typeOf (C.ERel pos e1 op e2) = do
    t1 <- typeOf e1
    checkTypeIs t1 IntT pos "Comparison"
    t2 <- typeOf e2
    checkTypeIs t2 IntT pos "Comparison"
    return BoolT

typeOf (C.EAnd pos e1 e2) = do
    t1 <- typeOf e1
    checkTypeIs t1 BoolT pos "Logical operation"
    t2 <- typeOf e2
    checkTypeIs t2 BoolT pos "Logical operation"
    return BoolT

typeOf (C.EOr pos e1 e2) = typeOf (C.EAnd pos e1 e2)

checkBlock :: C.Block -> TCMonad ()
checkBlock (C.Block pos []) = return ()
checkBlock (C.Block pos (st:stmts)) = do
    newEnv <- checkStatement st
    local (const newEnv) (checkBlock (C.Block pos stmts))

checkStatement :: C.Stmt -> TCMonad TCEnv
checkStatement (C.FnDef pos retType ident params block) = do
    curBlockVars <- asks blockVars
    if S.member ident curBlockVars then
        throwError $ Error "Cannot redefine function in the same block"  pos
    else do
        paramsSignatures <- mapM paramSignature params
        env <- ask
        let fnType = FunT (parseType retType) paramsSignatures
        let updatedEnvWithFunction = M.insert ident fnType (types env) -- env that includes this function's type
        fnBodyBlockVarsAndTypes <- createBlockVarsAndTypesFromParams params
        -- union is left biased
        local (const TCEnv {types = M.union (snd fnBodyBlockVarsAndTypes) updatedEnvWithFunction , blockVars = S.insert ident (fst fnBodyBlockVarsAndTypes), insideLoop = insideLoop env, insideFunc = Just fnType}) (checkBlock block)
        return $ env {types = updatedEnvWithFunction}

    where
        paramSignature :: C.Arg -> TCMonad (Type, Bool)
        paramSignature (C.ArgVal pos tp ident) = case parseType tp of
            FunT _ _ -> throwError $ Error "Impossible - Functions cannot be function parameters" pos
            goodType -> return (goodType, False)
        paramSignature (C.ArgRef pos tp ident) = do
            case parseType tp of
                FunT _ _ -> throwError $ Error "Impossible - Functions cannot be function parameters" pos
                ArrT _ -> throwError $ Error "Arrays are passed by reference by default - cannot get reference to reference" pos
                goodTp -> return (goodTp, True)

        -- goes over parameters one by one and prepares types and blockVars of env
        -- to be used when typechecking function body
        createBlockVarsAndTypesFromParams :: [C.Arg] -> TCMonad (Set C.Ident, Map C.Ident Type)
        createBlockVarsAndTypesFromParams [] = return (S.empty, M.empty)
        createBlockVarsAndTypesFromParams ((C.ArgVal pos tp ident):rest) = do
            curBlock <- createBlockVarsAndTypesFromParams rest
            if S.member ident (fst curBlock) then
                throwError $ Error "Function parameters doubled" pos
            else
                return (S.insert ident $ fst curBlock, M.insert ident (parseType tp) $ snd curBlock)
        createBlockVarsAndTypesFromParams ((C.ArgRef pos tp ident):rest) = createBlockVarsAndTypesFromParams ((C.ArgVal pos tp ident):rest)

checkStatement (C.Empty pos) = ask
checkStatement (C.BStmt pos block) = do
    env <- ask
    local (const env {blockVars = S.empty}) $ checkBlock block

    return env

checkStatement (C.Decl pos tp []) = ask

checkStatement (C.Decl pos tp (item:rest)) = do
    env <- ask
    let parsedTp = parseType tp

    case item of
        C.NoInit posDec ident -> if S.member ident $ blockVars env then
                                    throwError $ Error "Cannot redeclare a variable in the same block" posDec
                                else
                                    local (const env {types = M.insert ident parsedTp $ types env, blockVars = S.insert ident $ blockVars env}) $ checkStatement (C.Decl pos tp rest)
        C.Init posDec ident e -> if S.member ident $ blockVars env then
                                    throwError $ Error "Cannot redeclare a variable in the same block" posDec
                                else do
                                    eType <- typeOf e
                                    checkTypeIs eType parsedTp posDec "Declaration"
                                    local (const env {types = M.insert ident parsedTp $ types env, blockVars = S.insert ident $ blockVars env}) $ checkStatement (C.Decl pos tp rest)
checkStatement (C.VarAss pos ident expr) = do
    eType <- typeOf expr
    identType <- asks (M.lookup ident . types)
    case identType of
        Nothing -> throwError $ Error "Assignment to undeclared variable" pos
        Just tp -> if tp /= eType then throwError $ Error "Wrong expression type" pos else ask

checkStatement (C.ArrElAss pos ident indices expr) = do
    identType <- asks (M.lookup ident . types)
    exprType <- typeOf expr
    case identType of
        Nothing -> throwError $ Error "Assignment to undeclared variable" pos
        Just tp@(ArrT _) -> do
            arrDims <- getArrayDimSize tp pos
            indicesDims <- getArrayIndicesSize indices "Array assignment"
            if arrDims >= indicesDims then do
                checkTypeIs exprType (getNDimArrayT (getArrayBaseT tp) (arrDims - indicesDims)) pos "Array assignment"
                ask
            else
                throwError $ Error "Invalid array assignment" pos
        Just _notArray -> throwError $ Error ("Variable " ++ show ident ++ " is not an array") pos

checkStatement (C.Ret pos e) = do
    funcSig <- asks insideFunc
    retExprType <- typeOf e
    case funcSig of
        Nothing -> throwError $ Error "Return outside of function" pos
        Just (FunT retType paramSigs) -> if retType /= retExprType then throwError $ Error "Invalid return type"  pos else ask
        _notFunT -> throwError $ Error "Panic!" pos

checkStatement (C.Cond pos e stmt) = do 
    eType <- typeOf e
    checkTypeIs eType BoolT pos "If"
    checkStatement stmt
    ask

checkStatement (C.CondElse pos e st1 st2) = do
    eType <- typeOf e
    checkTypeIs eType BoolT pos "If Else"
    checkStatement st1
    checkStatement st2
    ask

checkStatement (C.While pos e stmt) = do
    env <- ask
    eType <- typeOf e
    checkTypeIs eType BoolT pos "While"
    local (const env {insideLoop = True}) (checkStatement stmt)
    return env

checkStatement (C.Break pos) = do
    isInLoop <- asks insideLoop
    if isInLoop then ask else throwError $ Error "Break outside of loop" pos
checkStatement (C.Cont pos) = do 
    isInLoop <- asks insideLoop
    if isInLoop then ask else throwError $ Error "Continue outside of loop" pos

checkStatement (C.Print pos e) = do
    typeOf e
    ask

checkStatement (C.SExp pos e ) = do
    typeOf e
    ask

runType :: C.Expr -> Either Error Type
runType e = runIdentity $ runExceptT $ runReaderT (typeOf e) emptyEnv
runTypeMonad x env = runIdentity $ runExceptT $ runReaderT x env

checkProgram :: C.Program -> TCMonad ()
checkProgram (C.Program pos stmts) = do
    checkBlock (C.Block pos stmts)

runTypeCheck :: C.Program -> Either Error ()
runTypeCheck program = runIdentity (runExceptT (runReaderT (checkProgram program) emptyEnv))