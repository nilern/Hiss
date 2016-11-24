> module Hiss.Data where
> import Control.Monad.Except (ExceptT, throwError, liftIO)
> import Control.Monad.State (StateT)
> import qualified Data.Map.Strict as Map
> import Data.Array
> import System.IO (Handle)
> import qualified Data.HashTable.IO as H

> type EvalState t = StateT (Toplevel, Store) (ExceptT SError IO) t

> liftThrows :: Either SError t -> EvalState t
> liftThrows (Left e) = throwError e
> liftThrows (Right v) = return v

= Value Representation

> type PureBuiltinImpl = [SValue] -> Either SError [SValue]
> type BuiltinImpl = [SValue] -> EvalState [SValue]

> data SValue = Symbol String
>             | Bool Bool
>             | Fixnum Int
>             | Pair SValue SValue
>             | Closure [String] (Maybe String) AST Env
>             | PureBuiltin PureBuiltinImpl
>             | Builtin BuiltinImpl
>             | Port Handle
>             | Nil
>             | Continuation Cont
>             | CallCC
>             | CallVs
>             | Apply
>             | Unspecified
>             | Unbound

> instance Show SValue where
>   show (Symbol cs) = cs
>   show (Bool True) = "#t"
>   show (Bool False) = "#f"
>   show (Fixnum n) = show n
>   show (Pair x xs) = '(' : show x ++ showElems xs
>       where showElems (Pair y ys) = ' ' : show y ++ showElems ys
>             showElems Nil = ")"
>             showElems y = " . " ++ show y ++ ")"
>   show (Closure _ _ _ _) = "#<lambda>"
>   show (Builtin _) = "#<lambda>"
>   show (PureBuiltin _) = "#<lambda>"
>   show (Port _) = "#<port>"
>   show Nil = "()"
>   show (Continuation _) = "#<lambda>"
>   show CallCC = "#<lambda>"
>   show CallVs = "#<lambda>"
>   show Apply = "#<lambda>"
>   show Unspecified = "#<unspecified>"
>   show Unbound = "#<unbound>"

> injectList :: [SValue] -> SValue
> injectList (v:vs) = Pair v (injectList vs)
> injectList [] = Nil

> ejectList :: SValue -> [SValue]
> ejectList (Pair x xs) = x : ejectList xs
> ejectList Nil = []

= Abstract Syntax Tree and Continuations

> data AST = Lambda [String] (Maybe String) AST
>          | Call AST [AST]
>          | If AST AST AST
>          | Begin [AST]
>          | Set String AST
>          | Var String
>          | Const SValue
>          deriving (Show)

> data Cont = Fn Cont Env [AST]
>           | Arg Cont Env SValue [SValue] [AST]
>           | AppVs Cont SValue
>           | Cond Cont Env AST AST
>           | SetName Cont Env String
>           | Began Cont Env [AST]
>           | Halt

= Environment

> type Address = Int
> type Env = Map.Map String Address
> type Toplevel = H.BasicHashTable String SValue

> emptyEnv :: Env
> emptyEnv = Map.empty

> lookup :: String -> Env -> Either SError Address
> lookup name e = case Map.lookup name e of
>                   Just a -> Right a
>                   Nothing -> Left $ Nonbound name

> insert :: String -> Address -> Env -> Env
> insert = Map.insert

> toplevelFromList :: [(String, SValue)] -> IO Toplevel
> toplevelFromList = H.fromList

> lookupGlobal :: String -> Toplevel -> ExceptT SError IO SValue
> lookupGlobal name eg = do ov <- liftIO $ H.lookup eg name
>                           case ov of
>                             Just v -> return v
>                             Nothing -> throwError $ Nonbound name

> setGlobal :: String -> SValue -> Toplevel -> ExceptT SError IO ()
> setGlobal name v eg = do ov <- liftIO $ H.lookup eg name
>                          case ov of
>                            Just _ -> liftIO $ H.insert eg name v
>                            Nothing -> throwError $ Nonbound name

= Store

> data Store = Store (Array Address SValue) Address

> emptyStore :: Store
> emptyStore = Store (listArray (0, 999999) (replicate 1000000 Unbound)) 0

> deref :: Address -> Store -> SValue
> deref a (Store vs _) = vs ! a

> alloc :: Store -> SValue -> (Address, Store) -- TODO: GC
> alloc (Store vs a) v = (a, Store (vs // [(a, v)]) (a + 1))

> set :: Address -> SValue -> Store -> Store
> set a v (Store vs n) = Store (vs // [(a, v)]) n

> def :: Env -> Store -> String -> SValue -> (Env, Store)
> def e s n v = (insert n a e, s')
>     where (a, s') = alloc s v

= Errors

> data SError = Nonbound String
>             | NonLambda SValue
>             | Argc
>             | Type
>             | NonError
>               deriving (Show)

> instance Monoid SError where
>   mempty = NonError
>   mappend _ e = e
