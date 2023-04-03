{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module TypeChecker where

import qualified Data.Map as M
import Data.Map(Map)

import qualified Data.Set as S
import Data.Set(Set)

import qualified Cimex.Abs as C
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity
import BNFC.Abs (BNFC'Position)

showPos :: BNFC'Position -> String
showPos (Just (line, col)) = show line ++ ":" ++ show col
showPos Nothing = "??:??"

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

emptyEnv :: TCEnv
emptyEnv = TCEnv {types = M.empty, blockVars = S.empty, insideLoop = False, insideFunc = Nothing}

type TCMonad a = (ReaderT TCEnv (ExceptT String Identity)) a

-- utility functions
checkTypeIsIn :: Type -> Set Type -> C.BNFC'Position ->  TCMonad ()
checkTypeIsIn t good pos = do
    if S.member t good then
        return ()
    else
        throwError $ "Type mismatch at " ++ showPos pos ++ "\n Expected types: " ++ showTypeOptions good ++ "\n Got: " ++ show t

checkTypeIs :: Type -> Type -> C.BNFC'Position ->  TCMonad ()
checkTypeIs t1 t2 = checkTypeIsIn t1 (S.singleton t2)

-- gets size  of dims and checks if it's a valid array type
getArrayDimSize :: Type -> C.BNFC'Position -> TCMonad Int
getArrayDimSize arr@(ArrT t) pos = go arr pos where
    go :: Type -> C.BNFC'Position -> TCMonad Int
    go typ position = case typ of
        FunT _ _ -> throwError $ "Invalid array type at: " ++ showPos position ++ " functions cannot be elements of an array"
        ArrT t2 -> do
            rest  <- go t2 position
            return $ rest + 1
        _basicType -> return 0
getArrayDimSize _notArray pos = throwError $ "Expected array at " ++ showPos pos

isArray :: Type -> Bool
isArray (ArrT _) = True
isArray _ = False

-- first arg is base type
createNDimArrayT :: Type -> Int -> Type
createNDimArrayT t 0 = t
createNDimArrayT t n = ArrT (createNDimArrayT t (n-1))

getArrayBaseT :: Type -> Type
getArrayBaseT (ArrT t) = getArrayBaseT t
getArrayBaseT t = t

-- checks if indices are integers and gets number of dimensions
getArrayIndicesSize :: [C.ArrArg] -> C.BNFC'Position -> TCMonad Int
getArrayIndicesSize [] pos = return 0
getArrayIndicesSize ((C.ArrIdx pos2 e):xs) pos1 = do
    t1 <- typeOf e
    checkTypeIs t1 IntT pos2
    rest <- getArrayIndicesSize xs pos2
    return $ rest + 1

typeOf :: C.Expr -> TCMonad Type
typeOf (C.ELitTrue pos) = return BoolT
typeOf (C.ELitFalse pos) = return BoolT
typeOf (C.ELitInt pos _) = return IntT
typeOf (C.EString pos _) = return StrT
typeOf (C.EVar pos v) = do
    typesEnv <- asks types
    case M.lookup v typesEnv of
        Nothing -> throwError $ "Usage of undeclared variable: " ++ showPos pos
        Just t -> return t

typeOf (C.Neg pos e) = do
    et <- typeOf e
    checkTypeIs et IntT pos
    return et

typeOf (C.Not pos e) = do
    et <- typeOf e
    checkTypeIs et BoolT pos
    return et

typeOf (C.ECrtArr pos t dims) = do
    let tp = parseType t
    typeDims <- getArrayDimSize tp pos
    indicesDims <- getArrayIndicesSize dims pos
    if typeDims == indicesDims then
        return tp
    else
        throwError $ "Invalid dimensions when creating an array on: " ++ showPos pos

typeOf (C.EApp pos ident args) = do
    eType <- asks (M.lookup ident . types)
    case eType of
        Just (FunT retType params) -> do
            argsSigs <- mapM getSignature args
            assertArgsMatchParams argsSigs params pos
            return retType
        Just _ -> throwError $ "Cannot perform function application on not function type at: " ++ showPos pos
        Nothing -> throwError $ "Function with identifier " ++ show ident ++ " doesn't exist (" ++ showPos pos ++ ")"
    where
        getSignature :: C.Expr -> TCMonad (Type, Bool, C.BNFC'Position)
        getSignature arg = do
            t <- typeOf arg
            let canBeRef = case arg of {C.EVar pos name -> True; _ -> False}
            return (t, canBeRef, pos)

        assertArgsMatchParams :: [(Type, Bool, C.BNFC'Position)] -> [(Type, Bool)] -> C.BNFC'Position -> TCMonad ()
        assertArgsMatchParams [] [] appPos = return ()
        assertArgsMatchParams argsSigs [] appPos = throwError $ "Too many arguments in function application at: " ++ showPos appPos
        assertArgsMatchParams [] params appPos = throwError $ "Not enough arguments in function application at: " ++ showPos appPos
        assertArgsMatchParams ((t1, canRef, argPos):r1) ((t2, shouldBeRef):r2) appPos
          | t1 /= t2 =
            throwError $ "Argument type does not match parameter type at: " ++ showPos argPos
          | not canRef && shouldBeRef =
            throwError $ "Argument needs to be a variable (reference error) at: " ++ showPos argPos
          | otherwise =
            assertArgsMatchParams r1 r2 appPos

typeOf (C.EArrGet pos ident idxs) = do
    tp <- asks (M.lookup ident . types)
    case tp of
        Nothing -> throwError $ "Undeclared array at: " ++ showPos pos
        Just arr -> do
            typeDims <- getArrayDimSize arr pos
            indicesDims <- getArrayIndicesSize idxs pos
            if typeDims >= indicesDims then
                return $ createNDimArrayT (getArrayBaseT arr) (typeDims - indicesDims)
            else
                throwError $ "Too many indices for this array at: " ++ showPos pos

typeOf (C.EAdd pos e1 op e2) = do
    t1 <- typeOf e1
    checkTypeIs t1 IntT pos
    t2 <- typeOf e2
    checkTypeIs t2 IntT pos
    return IntT

typeOf (C.EMul pos e1 op e2) = typeOf (C.EAdd pos e1 (C.Plus Nothing) e2)

typeOf (C.ERel pos e1 (C.EQU relpos) e2) = do
    t1 <- typeOf e1
    t2 <- typeOf e2
    if t1 == t2 then
        return BoolT
    else
        throwError $ "Comparison on two different types at: " ++ showPos pos

typeOf (C.ERel pos e1 op e2) = do
    t1 <- typeOf e1
    checkTypeIs t1 IntT pos
    t2 <- typeOf e2
    checkTypeIs t2 IntT pos
    return BoolT

typeOf (C.EAnd pos e1 e2) = do
    t1 <- typeOf e1
    checkTypeIs t1 BoolT pos
    t2 <- typeOf e2
    checkTypeIs t2 BoolT pos
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
        throwError $ "Cannot redefine function in the same block at: " ++ showPos pos
    else do
        paramsSignatures <- mapM paramSignature params
        env <- ask
        let fnType = FunT (parseType retType) paramsSignatures
        let typesWithFunction = M.insert ident fnType (types env)
        fnBodyBlockVarsAndTypes <- createBlockVarsAndTypesFromParams params
        local (const TCEnv {types = M.union (snd fnBodyBlockVarsAndTypes) typesWithFunction , blockVars = fst fnBodyBlockVarsAndTypes, insideLoop = insideLoop env, insideFunc = Just fnType}) (checkBlock block)
        return (TCEnv {types = typesWithFunction, blockVars = blockVars env, insideLoop = insideLoop env, insideFunc = insideFunc env})

    where
        paramSignature :: C.Arg -> TCMonad (Type, Bool)
        paramSignature (C.ArgVal pos tp ident) = case parseType tp of
            FunT _ _ -> throwError $ "Functions cannot be function parameters at: " ++ showPos pos
            goodType -> return (goodType, False)
        paramSignature (C.ArgRef pos tp ident) = do
            case parseType tp of
                FunT _ _ -> throwError $ "Functions cannot be function parameters at: " ++ showPos pos
                ArrT _ -> throwError $ "Arrays are passed by reference only - cannot get reference to reference at: " ++ showPos pos
                goodTp -> return (goodTp, True)
        createBlockVarsAndTypesFromParams :: [C.Arg] -> TCMonad (Set C.Ident, Map C.Ident Type)
        createBlockVarsAndTypesFromParams [] = return (S.empty, M.empty)
        createBlockVarsAndTypesFromParams ((C.ArgVal pos tp ident):rest) = do
            curBlock <- createBlockVarsAndTypesFromParams rest
            if S.member ident (fst curBlock) then
                throwError $ "Function parameters doubled at: " ++ showPos pos
            else
                return (S.insert ident $ fst curBlock, M.insert ident (parseType tp) $ snd curBlock)
        createBlockVarsAndTypesFromParams ((C.ArgRef pos tp ident):rest) = createBlockVarsAndTypesFromParams ((C.ArgVal pos tp ident):rest)

checkStatement (C.Empty pos) = ask
checkStatement (C.BStmt pos block) = do
    env <- ask
    local (const TCEnv {types = types env, blockVars = S.empty, insideLoop = insideLoop env, insideFunc = insideFunc env}) $ checkBlock block
    return env
checkStatement (C.Decl pos tp []) = ask
checkStatement (C.Decl pos tp (item:rest)) = do
    env <- ask
    let gtp = parseType tp
    case item of
        C.NoInit posDec ident -> if S.member ident $ blockVars env then
                                    throwError $ "Cannot redeclare a variable in the same block at: " ++ showPos posDec
                                else
                                    local (const TCEnv {types = M.insert ident gtp $ types env, blockVars = S.insert ident $ blockVars env, insideLoop = insideLoop env, insideFunc = insideFunc env}) $ checkStatement (C.Decl pos tp rest)
        C.Init posDec ident e -> if S.member ident $ blockVars env then
                                    throwError $ "Cannot redeclare a variable in the same block at: " ++ showPos posDec
                                else do
                                    eType <- typeOf e
                                    checkTypeIs eType gtp posDec
                                    local (const TCEnv {types = M.insert ident gtp $ types env, blockVars = S.insert ident $ blockVars env, insideLoop = insideLoop env, insideFunc = insideFunc env}) $ checkStatement (C.Decl pos tp rest)
checkStatement (C.VarAss pos ident expr) = do
    eType <- typeOf expr
    identType <- asks (M.lookup ident . types)
    case identType of
        Nothing -> throwError $ "Assignment to undeclared variable at: " ++ showPos pos
        Just tp -> if tp /= eType then throwError $ "Wrong expression type at: " ++ showPos pos else ask

checkStatement (C.ArrElAss pos ident indices expr) = do
    identType <- asks (M.lookup ident . types)
    exprType <- typeOf expr
    case identType of
        Nothing -> throwError $ "Assignment to undeclared variable at: " ++ showPos pos
        Just tp -> do
            arrDims <- getArrayDimSize tp pos
            indicesDims <- getArrayIndicesSize indices pos
            if arrDims >= indicesDims then do
                checkTypeIs exprType (createNDimArrayT (getArrayBaseT tp) (arrDims - indicesDims)) pos
                ask
            else
                throwError $ "Invalid array assignment at: " ++ showPos pos

checkStatement (C.Ret pos e) = do
    funcSig <- asks insideFunc
    retExprType <- typeOf e
    case funcSig of
        Nothing -> throwError $ "Return outside of function at: " ++ showPos pos
        Just (FunT retType paramSigs) -> if retType /= retExprType then throwError ("Invalid return type at: " ++ showPos pos) else ask

checkStatement (C.Cond pos e stmt) = do 
    eType <- typeOf e
    checkTypeIs eType BoolT pos
    checkStatement stmt
    ask

checkStatement (C.CondElse pos e st1 st2) = do
    eType <- typeOf e
    checkTypeIs eType BoolT pos
    checkStatement st1
    checkStatement st2
    ask
checkStatement (C.While pos e stmt) = do
    env <- ask
    eType <- typeOf e
    checkTypeIs eType BoolT pos
    local (const TCEnv {types = types env, blockVars = blockVars env, insideLoop = True, insideFunc = insideFunc env}) (checkStatement stmt)
    ask
checkStatement (C.Break pos) = do
    isInLoop <- asks (insideLoop)
    if isInLoop then ask else throwError $ "break outside of loop at: " ++ showPos pos
checkStatement (C.Cont pos) = do 
    isInLoop <- asks (insideLoop)
    if isInLoop then ask else throwError $ "continue outside of loop at: " ++ showPos pos

checkStatement (C.Print pos e) = do
    typeOf e
    ask

checkStatement (C.SExp pos e ) = do
    typeOf e
    ask

runType :: C.Expr -> Either String Type
runType e = runIdentity $ runExceptT $ runReaderT (typeOf e) emptyEnv
runTypeMonad x env = runIdentity $ runExceptT $ runReaderT x env

example :: Either String TCEnv
example = runTypeMonad (checkStatement (C.VarAss Nothing (C.Ident "x") (C.EVar Nothing (C.Ident "x")))) (TCEnv {types = M.fromList [(C.Ident "x", IntT), (C.Ident "y", BoolT)], blockVars = S.empty, insideFunc = Nothing, insideLoop = False})

checkProgram :: C.Program -> TCMonad ()
checkProgram (C.Program pos stmts) = do
    checkBlock (C.Block pos stmts)

runTypeCheck :: C.Program -> Either String ()
runTypeCheck program = runIdentity $ runExceptT $ runReaderT (checkProgram program) emptyEnv