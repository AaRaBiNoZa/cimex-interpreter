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
data Error = Error {desc :: String, location :: C.BNFC'Position}

type IEnv = Map C.Ident Loc

data Array a = Array {
    contents :: Map Int a,
    size :: Int
} deriving (Show, Eq, Ord)

incNewLock :: IState -> IState
incNewLock old = IState {store = store old, newloc = newloc old + 1}

createArray :: a -> Int -> Array a
createArray defaultValue size = (Array {contents = M.fromList [(idx, defaultValue) | idx <- [0..(size - 1)]], size = size})

allocArray :: C.BNFC'Position -> T.Type -> [Int] -> IMonad Loc
allocArray pos tp [] = error "CATASTROPHic error"
allocArray pos (T.ArrT (T.ArrT tp)) (s:rest) = do
    subArrayPtrs <- replicateM s (allocArray pos (T.ArrT tp) rest)

    let val = MultArrV $ Array {contents = M.fromList (zip [0..s - 1] subArrayPtrs), size=s}

    newLocation <- gets newloc
    modify incNewLock
    modify (\s -> IState {store = M.insert newLocation val $ store s, newloc = newloc s})

    return newLocation

allocArray pos tp (s:rest) = do
    let val = case tp of
            T.ArrT (T.IntT) -> IntArrV $ createArray 0 s
            T.ArrT (T.StrT) -> StrArrV $ createArray "" s
            T.ArrT (T.BoolT) -> BoolArrV $ createArray False s
    
    newLocation <- gets newloc
    modify incNewLock
    modify (\s -> IState {store = M.insert newLocation val $ store s, newloc = newloc s})

    return newLocation


data FnArg = ArgVal C.Ident | ArgRef C.Ident
    deriving (Show, Eq, Ord)

data Function = Fn {
    args :: [FnArg],
    body :: C.Block
} deriving (Show, Eq, Ord)

data Value = StringV String
            | IntV Int
            | BoolV Bool
            | ArrPtr Loc
            | StrArrV (Array String)
            | BoolArrV (Array Bool)
            | IntArrV (Array Int)
            | ArrV (Array Value)
            | MultArrV (Array Loc)
            | FuncV Function
    deriving (Show, Eq, Ord)



type IMonad a = StateT IState (ReaderT IEnv (ExceptT Error IO)) a

getArrArgs :: [C.ArrArg] -> IMonad [Int]
getArrArgs = mapM evalArg where
    evalArg :: C.ArrArg -> IMonad Int
    evalArg (C.ArrIdx pos e) = do
        v1 <- eval e
        let IntV value = v1
        if value <= 0 then 
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
    ptr <- allocArray pos parsedTp parsedArgs
    return (ArrPtr ptr)

accessArray :: Loc -> [Int] -> IMonad Value
acccessArray loc [] = loc
accessArray loc (s:rest) = do


eval (C.EArrGet pos ident args) = do
    parsedArgs <- getArrArgs args


