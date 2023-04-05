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

type IEnv = Map C.Ident Loc

data Array a = Array {
    contents :: Map Int a,
    size :: Int
} deriving (Show, Eq, Ord)

incNewLock :: IState -> IState
incNewLock old = IState {store = store old, newloc = newloc old + 1}

createArray :: a -> Int -> Array a
createArray defaultValue size = (Array {contents = M.fromList [(idx, defaultValue) | idx <- [0..(size - 1)]], size = size})

allocArray :: C.BNFC'Position -> T.Type -> [Int] -> IMonad Value
allocArray pos tp [] = error "CATASTROPHic error"
allocArray pos (T.ArrT (T.ArrT tp)) (s:rest) = do
    subArrayPtrs <- replicateM s (allocArray pos (T.ArrT tp) rest)

    let val = ArrV $ Array {contents = M.fromList (zip [0..s - 1] subArrayPtrs), size=s}

    newLocation <- gets newloc
    modify incNewLock
    modify (\s -> IState {store = M.insert newLocation val $ store s, newloc = newloc s})

    return (ArrPtr newLocation)

allocArray pos tp (s:rest) = do
    let val = case tp of
            T.ArrT (T.IntT) -> ArrV $ createArray (IntV 0) s
            T.ArrT (T.StrT) -> ArrV $ createArray (StrV "") s
            T.ArrT (T.BoolT) -> ArrV $ createArray (BoolV False) s

    newLocation <- gets newloc
    modify incNewLock
    modify (\s -> IState {store = M.insert newLocation val $ store s, newloc = newloc s})

    return (ArrPtr newLocation)


accessArray :: Value -> [Int] -> IMonad Value
acccessArray ptrOrVal [] = return ptrOrVal
accessArray (ArrPtr loc) (s:rest) = do
    arrValue <- gets (M.lookup loc . store)
    let (ArrV arr) = fromJust arrValue

    let nextInLine = M.lookup s $ contents arr

    accessArray (fromJust nextInLine) rest
accessArray _ _ = error "CATASTROPHIC"

withoutLast :: [Int] -> (Int, [Int])
withoutLast [] = (0, [])
withoutLast [x] = (x, [])
withoutLast (x:xs) = (l, x:t) where (l, t) = withoutLast xs

data FnArg = ArgVal C.Ident | ArgRef C.Ident
    deriving (Show, Eq, Ord)

data Function = Fn {
    args :: [FnArg],
    body :: C.Block,
    env :: IEnv
} deriving (Show, Eq, Ord)

data Value = StrV String
            | IntV Int
            | BoolV Bool
            | ArrPtr Loc
            | ArrV (Array Value)
            | FuncV Function
    deriving (Show, Eq, Ord)



type IMonad a = StateT IState (ReaderT IEnv (ExceptT Error IO)) a

data StmtResFlag = RetF Value | BreakF | ContF | NoF
data StmtReturn = StmtReturn {retEnv :: IEnv, flag :: StmtResFlag}

getArrArgs :: [C.ArrArg] -> IMonad [Int]
getArrArgs = mapM evalArg where
    evalArg :: C.ArrArg -> IMonad Int
    evalArg (C.ArrIdx pos e) = do
        v1 <- eval e
        let IntV value = v1
        if value < 0 then
            throwError $ Error {desc = "Cannot create array with one dimension less or equal to 0", location = pos}
        else
            return value


eval :: C.Expr -> IMonad Value
eval (C.EVar pos ident) = do
    loc <- asks (M.lookup ident)
    val <- gets (M.lookup (fromJust loc) . store)
    return $ fromJust val

eval (C.ELitInt pos int) = return (IntV (fromInteger int))
eval (C.ELitTrue pos) = return (BoolV True)
eval (C.ELitFalse pos) = return (BoolV False)
eval (C.Neg pos e) = do
    v <- eval e
    let (IntV value) = v
    return $ IntV (- value)
eval (C.Not pos e) = do
    v <- eval e
    let (BoolV bool) = v
    return $ BoolV (not bool)
eval (C.EMul pos e1 op e2) = do
    v1 <- eval e1
    v2 <- eval e2
    let (IntV value1) = v1
    let (IntV value2) = v2

    case op of
        C.Times _ -> return $ IntV (value1 * value2)
        C.Div _ -> return $ IntV (value1 `div` value2)
        C.Mod _ -> return $ IntV (value1 `mod` value2)

eval (C.EAdd pos e1 op e2) = do
    v1 <- eval e1
    v2 <- eval e2
    let (IntV value1) = v1
    let (IntV value2) = v2

    case op of
        C.Plus _ -> return $ IntV (value1 + value2)
        C.Minus _ -> return $ IntV (value1 - value2)

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
    v1 <- eval e1
    v2 <- eval e2

    let (BoolV bool1) = v1
    let (BoolV bool2) = v2

    return $ BoolV (bool1 && bool2)


eval (C.EOr pos e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2

    let (BoolV bool1) = v1
    let (BoolV bool2) = v2

    return $ BoolV (bool1 || bool2)

eval (C.ECrtArr pos tp args) = do
    let parsedTp = parseType tp
    parsedArgs <- getArrArgs args
    allocArray pos parsedTp parsedArgs


eval (C.EArrGet pos ident args) = do
    parsedArgs <- getArrArgs args
    Just loc <- asks (M.lookup ident)
    acccessArray (ArrPtr loc) parsedArgs

eval (C.EString pos str) = return $ StrV str

-- eval (C.EApp pos ident fnArgs) = do
--     Just functionLoc <- asks (M.lookup ident)
--     Just (Function f) <- gets (M.lookup functionLoc . store)



execBlock :: C.Block -> IMonad StmtResFlag

execBlock (C.Block pos []) = return NoF
execBlock  (C.Block pos (s:stmts)) = do
    newEnvAndF <- exec s
    case flag newEnvAndF of
        NoF -> local (const (retEnv newEnvAndF)) (execBlock $ C.Block pos stmts)
        validFlag -> return validFlag

exec :: C.Stmt -> IMonad StmtReturn
exec (C.Empty pos) = do
    env <- ask
    return (StmtReturn {retEnv = env, flag = NoF})

exec (C.BStmt pos block) = do
    env <- ask
    flag <- execBlock block
    return (StmtReturn {retEnv = env, flag = flag})

exec (C.Decl pos tp []) = do
    env <- ask
    return (StmtReturn env NoF)
exec (C.Decl pos tp (i:its)) = do
    let parsedType = parseType tp
    env <- ask
    val <- case (parsedType, i) of
            (_, C.Init pos ident expr) -> (eval expr)
            (T.StrT, _) -> return $ StrV ""
            (T.IntT, _) -> return $ IntV 0
            (T.BoolT, _) -> return $ BoolV False
            (T.ArrT _, _) -> return $ ArrPtr 0
            _ -> error "CATASTROPHIC"
    let ident = case (i) of
            C.Init pos iden expr -> iden
            C.NoInit pos iden -> iden

    newLocation <- gets newloc
    modify incNewLock
    let newEnv = M.insert ident newLocation env
    modify (\s ->IState {store = M.insert newLocation val $ store s, newloc = newloc s})

    return StmtReturn {retEnv = newEnv, flag = NoF}

exec (C.VarAss pos ident expr) = do
    env <- ask
    val <- eval expr
    valLoc <- asks (M.lookup ident)
    let valAddr = fromJust valLoc


    modify (\s -> IState {store = M.insert valAddr val (store s), newloc = newloc s})
    return (StmtReturn env NoF)

exec (C.Ret pos expr) = do
    value <- eval expr
    env <- ask

    return (StmtReturn env $ RetF value)

exec (C.Cond pos expr stmt) = do
    env <- ask
    (BoolV bool) <- eval expr

    if bool then
        exec stmt
    else
        return (StmtReturn env NoF)

exec (C.CondElse pos expr st1 st2) = do
    env <- ask
    (BoolV bool) <- eval expr

    if bool then
        exec st1
    else
        exec st2

exec (C.Break pos) = do
    env <- ask

    return (StmtReturn env BreakF)

exec (C.Cont pos) = do
    env <- ask

    return (StmtReturn env ContF)

exec (C.While pos expr st) = do
    oldEnv <- ask
    (BoolV bool) <- eval expr

    if bool then
        do
            StmtReturn env flag <- exec st
            case flag of
                BreakF -> return $ StmtReturn env NoF
                RetF _ -> return $ StmtReturn env flag
                noneOrCont -> exec (C.While pos expr st)
    else
        return (StmtReturn oldEnv NoF)


exec (C.SExp pos expr) = do
    val <- eval expr
    oldEnv <- ask

    return (StmtReturn oldEnv NoF)

exec (C.Print pos expr) = do
    val <- eval expr
    oldEnv <- ask

    liftIO $ print val

    return (StmtReturn oldEnv NoF)

exec (C.ArrElAss pos ident idxs expr) = do
    val <- eval expr
    oldEnv <- ask
    indexes <- getArrArgs idxs
    let (last, t) = withoutLast indexes 
    Just arrLoc <- asks (M.lookup ident)
    ArrPtr arrPtr <- accessArray (ArrPtr arrLoc) t

    Just (ArrV arrVal) <- gets (M.lookup arrPtr . store)
    let newArr = ArrV $ Array (M.insert last val $ contents arrVal) (size arrVal)

    modify (\s -> IState {store = M.insert arrPtr newArr (store s), newloc = newloc s})

    return $ StmtReturn oldEnv NoF


    
-- exec (C.FnDef pos tp ident args block) = do
    -- oldEnv <- ask
    -- newLocation <- gets newloc
    -- let newEnv = M.insert ident newLocation oldEnv

    -- modify incNewLock
    -- modify (\s -> IState {store = M.insert newLocation (FuncV (Fn args block oldEnv)) (store s), newloc =  newloc s})

    -- return (StmtReturn oldEnv NoF)
        

execProgram :: C.Program -> IMonad ()
execProgram (C.Program pos block) = do 
    execBlock (C.Block pos block)
    return ()

runProgram :: C.Program -> IO ()--(Either Error String)
runProgram prog = do
    res <- runExceptT (runReaderT (runStateT (execProgram prog) (IState {store = M.empty, newloc = 0})) M.empty )
    print res
    return ()