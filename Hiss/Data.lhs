> {-# LANGUAGE BangPatterns #-}

> module Hiss.Data where
> import System.Mem.StableName
> import System.IO.Unsafe (unsafePerformIO)
> import Control.Monad.Except (ExceptT, throwError, liftIO)
> import Control.Monad.State (StateT, get, put)
> import qualified Data.Map.Strict as Map
> import qualified Data.Set as Set
> import Data.Array
> import System.IO (Handle)
> import qualified Data.HashTable.IO as H
> import Text.Parsec.Pos (SourcePos, initialPos)

> type EvalState t = StateT (Toplevel, Store, SourcePos) (ExceptT SError IO) t

> liftThrows :: Either SError t -> EvalState t
> liftThrows (Left e) = throwError e
> liftThrows (Right v) = return v

> getPos :: EvalState SourcePos
> getPos = do (_, _, pos) <- get
>             return pos

> putPos :: SourcePos -> EvalState ()
> putPos pos = do (tl, s, _) <- get
>                 put (tl, s, pos)

= Value Representation

> type PrimopImpl = [SValue] -> EvalState [SValue]
> type ApplierImpl = Cont -> [SValue] -> EvalState (Cont, SValue, [SValue])
> type Context = Map.Map Int (Set.Set SValue)

> data SValue = Symbol String
>             | String String
>             | Fixnum Int
>             | Bool Bool
>             | Pair SValue SValue
>             | Nil
>             | Syntax SValue Context SourcePos
>             | Closure [String] (Maybe String) AST Env
>             | Continuation Cont
>             | Port Handle
>             | Unbound
>             | Unspecified

> instance Eq SValue where
>   (Symbol a) == (Symbol b) = a == b
>   (String a) == (String b) = a == b
>   (Fixnum a) == (Fixnum b) = a == b
>   (Bool a) == (Bool b) = a == b
>   (Pair a b) == (Pair c d) = a == c && b == d
>   Nil == Nil = True
>   (Syntax a aCtx aPos) == (Syntax b bCtx bPos) =
>       a == b && aCtx == bCtx && aPos == bPos
>   (!a @ (Closure _ _ _ _)) == (!b @ (Closure _ _ _ _)) =
>       unsafePerformIO $ (==) <$> makeStableName a <*> makeStableName b
>   (!k @ (Continuation _)) == (!l @ (Continuation _)) =
>       unsafePerformIO $ (==) <$> makeStableName k <*> makeStableName l
>   (Port h1) == (Port h2) = h1 == h2
>   Unbound == Unbound = True
>   Unspecified == Unspecified = True
>   _ == _ = False

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
>   show (Syntax _ _ _) = "#<syntax object>"
>   show (Closure _ _ _ _) = "#<lambda>"
>   show (Continuation _) = "#<lambda>"
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

> class Positioned a where
>   positionOf :: a -> SourcePos

> data AST = Lambda SourcePos [String] (Maybe String) AST
>          | Call SourcePos AST [AST]
>          | Primop SourcePos Primop [AST]
>          | If SourcePos AST AST AST
>          | Case SourcePos AST [(AST, AST)] (Maybe AST)
>          | Begin SourcePos [AST]
>          | Set SourcePos String AST
>          | Var SourcePos String
>          | Const SourcePos SValue

> instance Positioned AST where
>   positionOf (Lambda pos _ _ _) = pos
>   positionOf (Call pos _ _) = pos
>   positionOf (Primop pos _ _) = pos
>   positionOf (If pos _ _ _) = pos
>   positionOf (Begin pos _) = pos
>   positionOf (Set pos _ _) = pos
>   positionOf (Var pos _) = pos
>   positionOf (Const pos _) = pos

> data Primop = Impure PrimopImpl
>             | Applier ApplierImpl

> data Cont = Fn SourcePos Cont Env [AST]
>           | Arg SourcePos Cont Env SValue [SValue] [AST]
>           | PrimArg SourcePos Cont Env Primop [SValue] [AST]
>           | AppVs SourcePos Cont SValue
>           | Cond SourcePos Cont Env AST AST
>           | CaseDiscr SourcePos Cont Env [(AST, AST)] (Maybe AST)
>           | CaseCmp SourcePos Cont Env SValue AST [(AST, AST)] (Maybe AST)
>           | Began SourcePos Cont Env [AST]
>           | SetName SourcePos Cont Env String
>           | Halt SourcePos

> instance Positioned Cont where
>   positionOf (Fn pos _ _ _) = pos
>   positionOf (Arg pos _ _ _ _ _) = pos
>   positionOf (PrimArg pos _ _ _ _ _) = pos
>   positionOf (AppVs pos _ _) = pos
>   positionOf (Cond pos _ _ _ _) = pos
>   positionOf (Began pos _ _ _) = pos
>   positionOf (SetName pos _ _ _) = pos
>   positionOf (Halt pos) = pos

= Environment

> type Address = Int
> type Env = Map.Map String Address
> type Toplevel = H.BasicHashTable String SValue

> emptyEnv :: Env
> emptyEnv = Map.empty

> lookup :: String -> Env -> SourcePos -> Either SError Address
> lookup name e pos = case Map.lookup name e of
>                       Just a -> Right a
>                       Nothing -> Left $ Nonbound pos name

> insert :: String -> Address -> Env -> Env
> insert = Map.insert

> toplevelFromList :: [(String, SValue)] -> IO Toplevel
> toplevelFromList = H.fromList

> lookupGlobal :: String -> Toplevel -> SourcePos -> ExceptT SError IO SValue
> lookupGlobal name eg pos = do ov <- liftIO $ H.lookup eg name
>                               case ov of
>                                 Just v -> return v
>                                 Nothing -> throwError $ Nonbound pos name

> setGlobal :: String -> SValue -> Toplevel -> SourcePos -> ExceptT SError IO ()
> setGlobal name v eg pos = do ov <- liftIO $ H.lookup eg name
>                              case ov of
>                                Just _ -> liftIO $ H.insert eg name v
>                                Nothing -> throwError $ Nonbound pos name

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

> data SError = Nonbound SourcePos String
>             | NonLambda SourcePos SValue
>             | Argc SourcePos
>             | Type SourcePos
>             | NilLiteral SourcePos
>             | NonError SourcePos
>               deriving (Show)

> instance Monoid SError where
>   mempty = NonError $ initialPos "__no-file__"
>   mappend _ e = e
