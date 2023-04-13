module Interpreter where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Monad.Except

import qualified Data.Map as M
import Data.Map(Map)

import qualified Cimex.Abs as C
import Data.Maybe (fromJust)

import qualified TypeChecker as T
import TypeChecker (parseType)
import Control.Monad (replicateM)

type Loc = Int

data IState = IState {store :: Map Loc Value, newloc :: Loc} deriving Show
data Error = Error {desc :: String, location :: C.BNFC'Position} deriving Show

data IEnv = Env {env :: Map C.Ident Loc, flag :: Flag} deriving (Show, Eq, Ord)

data Flag = RetF Value | BreakF | ContF | NoF deriving (Show, Eq, Ord)

data Array a = Array {
    contents :: Map Int a,
    size :: Int
} deriving (Show, Eq, Ord)


data FnArg = ArgVal C.Ident | ArgRef C.Ident
    deriving (Show, Eq, Ord)

data Function = Fn {
    params :: [FnArg],
    body :: C.Block,
    frozenEnv :: IEnv,
    defaultValue :: Value
} deriving (Show, Eq, Ord)

data Value = StrV String
            | IntV Int
            | BoolV Bool
            | ArrPtr Loc
            | ArrV (Array Value)
            | FuncV Function
    deriving (Show, Eq, Ord)


type IMonad a = StateT IState (ReaderT IEnv (ExceptT Error IO)) a

newLoc :: IMonad Loc
newLoc = do
    newLocation <- gets newloc
    modify (\old -> old {newloc = newloc old + 1})
    return newLocation


incNewLock :: IState -> IState
incNewLock old = IState {store = store old, newloc = newloc old + 1}

createArray :: a -> Int -> Array a
createArray defaultValue size = (Array {contents = M.fromList [(idx, defaultValue) | idx <- [0..(size - 1)]], size = size})


getArrArgs :: [C.ArrArg] -> IMonad [Int]
getArrArgs = mapM evalArg where
    evalArg :: C.ArrArg -> IMonad Int
    evalArg (C.ArrIdx pos e) = do
        v1 <- eval e
        let IntV value = v1
        if value < 0 then
            throwError $ Error {desc = "Runtime Error: Cannot create array with one dimension less or equal to 0", location = pos}
        else
            return value


allocArray :: C.BNFC'Position -> T.Type -> [Int] -> IMonad Value
allocArray pos tp [] = throwError $ Error "Impossible - cannot create null dimensional array" pos
allocArray pos (T.ArrT tp) (s:rest) = do
    val <- case tp of
        T.IntT -> return (ArrV $ createArray (IntV 0) s)
        T.StrT -> return (ArrV $ createArray (StrV "") s)
        T.BoolT -> return (ArrV $ createArray (BoolV False) s)
        T.ArrT subTp -> do
            subArrayPtrs <- replicateM s (allocArray pos (T.ArrT subTp) rest)
            return (ArrV $ Array {contents = M.fromList (zip [0..s - 1] subArrayPtrs), size=s})
        _ -> throwError $ Error "Impossible - cannot create array of functions" pos

    newLocation <- gets newloc
    modify incNewLock
    modify (\s -> IState {store = M.insert newLocation val $ store s, newloc = newloc s})

    return (ArrPtr newLocation)

allocArray pos _ _ = throwError $ Error "Impossible - wrong type for alloc array" pos

accessArray :: Value -> [Int] -> C.BNFC'Position -> IMonad Value
accessArray ptrOrVal [] pos = return ptrOrVal
accessArray (ArrPtr (-1)) _ pos = throwError $ Error "Runtime error - trying to access uninitialized array" pos
accessArray (ArrPtr loc) (s:rest) pos = do
    Just (ArrV arr) <- gets (M.lookup loc . store)
    if s >= size arr
        then throwError $ Error "Runtime error - access past end of array" pos
        else do
            let Just next = M.lookup s $ contents arr

            accessArray next rest pos
accessArray _ _ pos = throwError $ Error "Impossible - invalid arrray access" pos

setArray :: Value -> [Int] -> Value -> C.BNFC'Position -> IMonad ()
setArray _ [] _ pos = throwError $ Error "Impossible - invalid array assign" pos
setArray (ArrPtr loc) [idx] val pos = do
    Just (ArrV arr) <- gets (M.lookup loc . store)
    let newArr = arr {contents = M.insert idx val (contents arr)}
    modify (\s -> s {store = M.insert loc (ArrV newArr) (store s)} )
setArray (ArrPtr loc) (i:idxs) val pos = do
    Just (ArrV arr) <- gets (M.lookup loc . store)
    let Just next = M.lookup i $ contents arr
    setArray next idxs val pos
setArray _ _ _ pos = throwError $ Error "Impossible - invalid array assign" pos

eval :: C.Expr -> IMonad Value
eval (C.EVar pos ident) = do
    Just loc <- asks (M.lookup ident . env)
    Just val <- gets (M.lookup loc . store)
    return val

eval (C.ELitInt pos int) = return (IntV (fromInteger int))
eval (C.ELitTrue pos) = return (BoolV True)
eval (C.ELitFalse pos) = return (BoolV False)
eval (C.Neg pos e) = do
    IntV v <- eval e
    return $ IntV (- v)
eval (C.Not pos e) = do
    BoolV bool <- eval e
    return $ BoolV (not bool)
eval (C.EMul pos e1 op e2) = do
    IntV v1 <- eval e1
    IntV v2 <- eval e2

    case op of
        C.Times _ -> return $ IntV (v1 * v2)
        C.Div _ -> return $ IntV (v1 `div` v2)
        C.Mod _ -> return $ IntV (v1 `mod` v2)

eval (C.EAdd pos e1 op e2) = do
    IntV v1 <- eval e1
    IntV v2 <- eval e2

    case op of
        C.Plus _ -> return $ IntV (v1 + v2)
        C.Minus _ -> return $ IntV (v1 - v2)

eval (C.ERel pos e1 op e2) = do
    v1 <- eval e1
    v2 <- eval e2

    let res = case op of
                C.LTH _ -> v1 < v2
                C.LE _ -> v1 <= v2
                C.GTH _ -> v1 > v2
                C.GE _ -> v1 >= v2
                C.NE _ -> v1 /= v2
                C.EQU _ -> v1 == v2

    return $ BoolV res

eval (C.EAnd pos e1 e2) = do
    BoolV bool1 <- eval e1
    BoolV bool2 <- eval e2

    return $ BoolV (bool1 && bool2)


eval (C.EOr pos e1 e2) = do
    BoolV bool1 <- eval e1
    BoolV bool2 <- eval e2

    return $ BoolV (bool1 || bool2)

eval (C.ECrtArr pos tp args) = do
    let parsedTp = parseType tp
    parsedArgs <- getArrArgs args
    allocArray pos parsedTp parsedArgs


eval (C.EArrGet pos ident args) = do
    parsedArgs <- getArrArgs args
    Just loc <- asks (M.lookup ident . env)
    Just arrPtr <- gets (M.lookup loc . store)
    accessArray arrPtr parsedArgs pos

eval (C.EString pos str) = return $ StrV str

eval (C.EApp pos ident fnArgs) = do
    Just fcLoc <- asks (M.lookup ident . env)
    Just (FuncV fc) <- gets (M.lookup fcLoc . store)

    let fEnv = frozenEnv fc
    let envForRec = fEnv {env = M.insert ident fcLoc (env fEnv)}

    execEnv <- prepareEnvForFExecution pos (params fc) fnArgs envForRec

    result <- local (const execEnv) (execBlock (body fc))

    case result of
        RetF val -> return val
        NoF -> return $ defaultValue fc
        _ -> throwError $ Error "Impossible - break/continue got out of a function" pos
    where
        prepareEnvForFExecution :: C.BNFC'Position -> [FnArg] -> [C.Expr] -> IEnv -> IMonad IEnv
        prepareEnvForFExecution pos [] [] oldEnv = return oldEnv
        prepareEnvForFExecution pos (param:prest) (arg:argrest) oldEnv = do
            newEnv <- case (param, arg) of
                    (ArgVal ident, _) -> do
                        val <- eval arg

                        newLocation <- gets newloc
                        modify incNewLock
                        modify (\s -> s {store = M.insert newLocation val (store s)})

                        return $ oldEnv {env = M.insert ident newLocation (env oldEnv)}
                    (ArgRef ident, C.EVar pos refIdent) -> do
                        Just ref <- asks (M.lookup refIdent . env)

                        return $ oldEnv {env = M.insert ident ref (env oldEnv)}
                    (_, _) -> throwError $ Error "Impossible - invalid function execution" pos

            prepareEnvForFExecution pos prest argrest newEnv
        prepareEnvForFExecution pos _ _ _ = throwError $ Error "Impossible - arguments/parameters don't match" pos
    






execBlock :: C.Block -> IMonad Flag

execBlock (C.Block pos []) = return NoF
execBlock  (C.Block pos (s:stmts)) = do
    newEnv <- exec s
    case flag newEnv of
        NoF -> local (const newEnv) (execBlock $ C.Block pos stmts)
        validFlag -> return validFlag

exec :: C.Stmt -> IMonad IEnv
exec (C.Empty pos) = ask

exec (C.BStmt pos block) = do
    e <- ask
    flag <- execBlock block
    return e {flag = flag}

exec (C.Decl pos tp []) = ask
exec (C.Decl pos tp (i:its)) = do
    let parsedType = parseType tp
    val <- case (parsedType, i) of
            (_, C.Init pos ident expr) -> eval expr
            (T.StrT, _) -> return $ StrV ""
            (T.IntT, _) -> return $ IntV 0
            (T.BoolT, _) -> return $ BoolV False
            (T.ArrT _, _) -> return $ ArrPtr 0
            (_, _) -> throwError $ Error "Impossible - declaring a function somehow" pos
    let ident = case i of
            C.Init pos iden expr -> iden
            C.NoInit pos iden -> iden

    e <- ask
    newLocation <- gets newloc
    modify incNewLock
    let newEnv = M.insert ident newLocation (env e)
    modify (\s ->IState {store = M.insert newLocation val $ store s, newloc = newloc s})

    local (const (e {env = newEnv})) $ exec (C.Decl pos tp its)

exec (C.VarAss pos ident expr) = do
    val <- eval expr
    Just valLoc <- asks (M.lookup ident . env)

    modify (\s -> IState {store = M.insert valLoc val (store s), newloc = newloc s})
    ask

exec (C.Ret pos expr) = do
    value <- eval expr
    e <- ask

    return e {flag = RetF value}

exec (C.Cond pos expr stmt) = do
    (BoolV bool) <- eval expr

    if bool then
        exec stmt
    else
        ask

exec (C.CondElse pos expr st1 st2) = do
    (BoolV bool) <- eval expr

    if bool then
        exec st1
    else
        exec st2

exec (C.Break pos) = do
    e <- ask

    return e {flag = BreakF}

exec (C.Cont pos) = do
    e <- ask

    return e {flag = ContF}

exec (C.While pos expr st) = do
    (BoolV bool) <- eval expr
    e <- ask
    if bool then
        do
            newEnv <- exec st
            case flag newEnv of
                BreakF -> ask
                RetF v -> return $ e {flag = RetF v}
                noneOrCont -> exec (C.While pos expr st)
    else
        ask

exec (C.SExp pos expr) = do
    val <- eval expr
    ask

exec (C.Print pos expr) = do
    val <- eval expr

    liftIO $ print val

    ask

exec (C.ArrElAss pos ident idxs expr) = do
    val <- eval expr
    indices <- getArrArgs idxs

    Just loc <- asks (M.lookup ident . env)

    if loc == - 1 then
        throwError $ Error "Runtime error - assignment to uninitialized array" pos
    else do
        Just arrPtr <- gets (M.lookup loc . store)

        setArray arrPtr indices val pos

        ask


    
exec (C.FnDef pos tp ident params block) = do
    e <- ask
    let parsedParams = parseParams params
    defVal <- case parseType tp of 
            T.BoolT -> return $ BoolV False
            T.StrT -> return $ StrV ""
            T.IntT -> return $ IntV 0
            T.ArrT _ -> return $ ArrPtr $ -1
            _ -> throwError $ Error "Impossible - invalid fn type somehow" pos

    let fc = Fn {params = parsedParams, body = block, frozenEnv = e, defaultValue = defVal}

    newLocation <- gets newloc
    modify incNewLock
    modify (\s -> s {store = M.insert newLocation (FuncV fc) (store s)})
    return (e {env = M.insert ident newLocation (env e)}) where
        parseParams :: [C.Arg] -> [FnArg]
        parseParams = map go where
            go (C.ArgVal pos tp ident) = ArgVal ident
            go (C.ArgRef pos tp ident) = ArgRef ident


        

execProgram :: C.Program -> IMonad ()
execProgram (C.Program pos block) = do 
    execBlock (C.Block pos block)
    return ()

runProgram :: C.Program -> IO ()--(Either Error String)
runProgram prog = do
    res <- runExceptT (runReaderT (runStateT (execProgram prog) (IState {store = M.empty, newloc = 0})) Env {env = M.empty, flag=NoF} )
    print res
    return ()