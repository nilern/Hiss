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

> type PurePrimopImpl = [SValue] -> Either SError [SValue]
> type PrimopImpl = [SValue] -> EvalState [SValue]

> data SValue = Symbol String
>             | String String
>             | Fixnum Int
>             | Bool Bool
>             | Pair SValue SValue
>             | Nil
>             | Closure [String] (Maybe String) AST Env
>             | Continuation Cont
>             | Apply
>             | CallCC
>             | CallVs
>             | Port Handle
>             | Unbound
>             | Unspecified

> instance Show SValue where
>   show (Symbol cs) = cs
>   show (String s) = '"' : s ++ "\""
>   show (Fixnum n) = show n
>   show (Bool True) = "#t"
>   show (Bool False) = "#f"
>   show (Pair x xs) = '(' : show x ++ showElems xs
>       where showElems (Pair y ys) = ' ' : show y ++ showElems ys
>             showElems Nil = ")"
>             showElems y = " . " ++ show y ++ ")"
>   show Nil = "()"
>   show (Closure _ _ _ _) = "#<lambda>"
>   show (Continuation _) = "#<lambda>"
>   show Apply = "#<lambda>"
>   show CallCC = "#<lambda>"
>   show CallVs = "#<lambda>"
>   show (Port _) = "#<port>"
>   show Unbound = "#<unbound>"
>   show Unspecified = "#<unspecified>"

> injectList :: [SValue] -> SValue
> injectList (v:vs) = Pair v (injectList vs)
> injectList [] = Nil

> ejectList :: SValue -> [SValue]
> ejectList (Pair x xs) = x : ejectList xs
> ejectList Nil = []

= Abstract Syntax Tree and Continuations

> data AST = Lambda [String] (Maybe String) AST
>          | Call AST [AST]
>          | Primop Primop [AST]
>          | If AST AST AST
>          | Begin [AST]
>          | Set String AST
>          | Var String
>          | Const SValue

> data Primop = Pure PurePrimopImpl
>             | Impure PrimopImpl

> data Cont = Fn Cont Env [AST]
>           | Arg Cont Env SValue [SValue] [AST]
>           | PrimArg Cont Env Primop [SValue] [AST]
>           | AppVs Cont SValue
>           | Cond Cont Env AST AST
>           | Began Cont Env [AST]
>           | SetName Cont Env String
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
