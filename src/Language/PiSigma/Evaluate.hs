{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Language.PiSigma.Evaluate
  ( Eval (..)
  , catchE
  , decl
  , decl'
  , defn'
  , force
  , eval
  , evalProg
  , getEnv
  , getId
  , letn
  , letn'
  , lookupId
  , run
  , subst
  , tdecl )
  where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import Language.PiSigma.Syntax
import qualified Language.PiSigma.Util.String.Internal
  as Internal

-- * Monad used for evaluation

type EvalErr = Internal.String

instance Error EvalErr where


newtype Eval e a = Eval { unEval :: StateT e (ErrorT EvalErr Identity) a }
  deriving ( Monad
           , MonadError EvalErr
           , MonadState e )

run :: e -> Eval e a -> Either EvalErr (a, e)
run e (Eval p) = runIdentity $ runErrorT $ runStateT p e

catchE :: Eval e a -> (EvalErr -> Eval e a) -> Eval e a
catchE = catchError

getEnv :: Eval e e
getEnv = get

setEnv :: e -> Eval e ()
setEnv = put

getId :: Loc -> Name -> Scope -> Eval e Id
getId l x s = case lookupScope s x of
  Just i  -> return i
  Nothing -> throwError msg
    where
      msg = Internal.concat [ Internal.fromString $ locMessage l
                            , "\nUndefined variable: "
                            , x
                            , "\n"
                            ]

-- | Takes a name, a scope, and potentially a type.
-- It extends the environment and the scope with that name.
decl :: Env e
     => Name
     -> PrtInfo
     -> Scope
     -> Maybe  (Clos Type)
     -> Eval e (Id, Scope)
decl x x' s a =
    do e <- getEnv
       let (i,e') = extE e x'
       setEnv e'
       return (i,extendScope s x (i,a))

decl' :: Env e => Name -> Scope -> Eval e (Id, Scope)
decl' x s   = decl x PrtInfo{ name = x, expand = True } s Nothing

tdecl :: Env e => Name -> Scope -> (Clos Type) -> Eval e (Id, Scope)
tdecl x g = decl x PrtInfo{ name = x, expand = True } g . Just

-- | Updates the environment.
defn :: Env e => Id -> EnvEntry -> Eval e ()
defn i ei =
    do e <- getEnv
       setEnv (setE e i ei)

defn' :: Env e => Id -> Clos Type -> Eval e ()
defn' i = defn i . Closure

-- | Locally updates the environment.
letn :: Env e => Id -> EnvEntry -> Eval e a -> Eval e a
letn i ei p =
    do eo <- lookupId i
       defn i ei
       a  <- p
       defn i eo
       return a

letn' :: Env e => Id -> Clos Type -> Eval e a -> Eval e a
letn' i = letn i . Closure

subst :: Env e => Bind (Clos Term) -> (Clos Term) -> Eval e (Clos Term)
subst (x,(t,s)) u =
    do (i,s') <- decl' x s
       defn' i u
       return (t,s')

evalApp :: Env e => Val -> (Clos Term) -> Eval e Val
evalApp (VLam xt) u = eval =<< subst xt u
evalApp (Ne t)    u = return (Ne (t :.. u))
evalApp _         _ = throwError "function expected"

lookupId :: Env e => Id -> Eval e EnvEntry
lookupId i = liftM (flip getE i) getEnv

evalId :: Env e => Id -> Eval e Val
evalId i =
    do ei <- lookupId i
       case ei of
         Closure t -> eval t
         Id j      -> return (Ne (NVar j))

evalSplit :: Env e
          => Val
          -> Bind (Bind (Clos Term))
          -> Eval e Val
evalSplit (VPair ((l,r),s)) (x,(y,(t,s'))) =
    do ts2 <- subst (x, (t, s')) (l, s)
       eval =<< subst (y, ts2) (r, s)
evalSplit (Ne n) b = return (Ne (NSplit n b))
evalSplit _ _ = throwError "Pair expected"

evalCase :: Env e => Val -> Clos [(Label,Term)] -> Eval e Val
evalCase (VLabel l) (lts,s) =
    case lookup l lts of
      Nothing -> throwError "case not matched"
      Just t  -> eval (t,s)
evalCase (Ne n) lts = return (Ne (NCase n lts))
evalCase _ _ = throwError "Label expected"

force :: Env e => Val -> Eval e Val
force (VBox (Boxed c)) = eval c
force (Ne n) = return (Ne (NForce n))
force _ = throwError "Box expected"

unfold :: Env e => Val -> Bind (Clos Term) -> Eval e Val
unfold (VFold c) b = eval =<< subst b c
unfold (Ne n)    b = return (Ne (NUnfold n b))
unfold _         _ = throwError "Fold expected"

eval :: Env e => (Clos Term) -> Eval e Val
eval (Var l x, s)               = evalId =<< getId l x s
eval (Let _ g t, s)             = curry eval t =<< evalProg (g,s)
eval (Type _, _)                = return VType
eval (Q _ ps axb, s)            = return (VQ ps (axb,s))
eval (Lift _ t, s)              = return (VLift (t,s))
eval (Lam _ (x,t), s)           = return (VLam (x,(t,s)))
eval (App t u, s)               = flip evalApp (u,s) =<< eval (t,s)
eval (Pair _ t u, s)            = return (VPair ((t,u),s))
eval (Split _ t (x, (y, u)), s) = flip evalSplit (x,(y,(u,s))) =<< eval (t,s)
eval (Enum _ ls, _)             = return (VEnum ls)
eval (Label _ l, _)             = return (VLabel l)
eval (Case _ t lts, s)          = flip evalCase (lts,s) =<< eval (t,s)
eval (Box _ t, s)               = return (VBox (Boxed (t,s)))
eval (Force _ t, s)             = force =<< eval (t,s)
eval (Rec _ t, s)               = return (VRec (t,s))
eval (Fold _ t, s)              = return (VFold (t,s))
eval (Unfold _ t (x, u), s)     = flip unfold (x, (u, s)) =<< eval (t,s)

evalProg :: Env e => Clos Prog -> Eval e Scope
evalProg ([],s) = return s
evalProg ((Decl _ x _):tel, s) = do (_,s') <- decl x (PrtInfo x False) s Nothing
                                    evalProg (tel,s')
evalProg ((Defn l x t):tel, s) = do i <- getId l x s
                                    defn' i (t,s)
                                    evalProg (tel,s)
