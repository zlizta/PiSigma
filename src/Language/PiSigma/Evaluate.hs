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


newtype Eval a = Eval { unEval :: StateT EnvEntries (ErrorT EvalErr Identity) a }
  deriving ( Monad
           , MonadError EvalErr
           , MonadState EnvEntries )

run :: EnvEntries -> Eval a -> Either EvalErr (a, EnvEntries)
run e (Eval p) = runIdentity $ runErrorT $ runStateT p e

catchE :: Eval a -> (EvalErr -> Eval a) -> Eval a
catchE = catchError

getEnv :: Eval EnvEntries
getEnv = get

setEnv :: EnvEntries -> Eval ()
setEnv = put

getId :: Loc -> Name -> Scope -> Eval Id
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
decl :: Name
     -> PrtInfo
     -> Scope
     -> Maybe  (Clos Type)
     -> Eval (Id, Scope)
decl x x' s a =
    do e <- getEnv
       let (i,e') = extE e x'
       setEnv e'
       return (i,extendScope s x (i,a))

decl' :: Name -> Scope -> Eval (Id, Scope)
decl' x s   = decl x PrtInfo{ name = x, expand = True } s Nothing

tdecl :: Name -> Scope -> (Clos Type) -> Eval (Id, Scope)
tdecl x g = decl x PrtInfo{ name = x, expand = True } g . Just

-- | Updates the environment.
defn :: Id -> EnvEntry -> Eval ()
defn i ei =
    do e <- getEnv
       setEnv (setE e i ei)

defn' :: Id -> Clos Type -> Eval ()
defn' i = defn i . Closure

-- | Locally updates the environment.
letn :: Id -> EnvEntry -> Eval a -> Eval a
letn i ei p =
    do eo <- lookupId i
       defn i ei
       a  <- p
       defn i eo
       return a

letn' :: Id -> Clos Type -> Eval a -> Eval a
letn' i = letn i . Closure

subst :: Bind (Clos Term) -> (Clos Term) -> Eval (Clos Term)
subst (x,(t,s)) u =
    do (i,s') <- decl' x s
       defn' i u
       return (t,s')

evalApp :: Val -> (Clos Term) -> Eval Val
evalApp (VLam xt) u = eval =<< subst xt u
evalApp (Ne t)    u = return (Ne (t :.. u))
evalApp _         _ = throwError "function expected"

lookupId :: Id -> Eval EnvEntry
lookupId i = liftM (flip getE i) getEnv

evalId :: Id -> Eval Val
evalId i =
    do ei <- lookupId i
       case ei of
         Closure t -> eval t
         Id j      -> return (Ne (NVar j))

evalSplit :: Val
          -> Bind (Bind (Clos Term))
          -> Eval Val
evalSplit (VPair ((l,r),s)) (x,(y,(t,s'))) =
    do ts2 <- subst (x, (t, s')) (l, s)
       eval =<< subst (y, ts2) (r, s)
evalSplit (Ne n) b = return (Ne (NSplit n b))
evalSplit _ _ = throwError "Pair expected"

evalCase :: Val -> Clos [(Label,Term)] -> Eval Val
evalCase (VLabel l) (lts,s) =
    case lookup l lts of
      Nothing -> throwError "case not matched"
      Just t  -> eval (t,s)
evalCase (Ne n) lts = return (Ne (NCase n lts))
evalCase _ _ = throwError "Label expected"

force :: Val -> Eval Val
force (VBox (Boxed c)) = eval c
force (Ne n) = return (Ne (NForce n))
force _ = throwError "Box expected"

unfold :: Val -> Bind (Clos Term) -> Eval Val
unfold (VFold c) b = eval =<< subst b c
unfold (Ne n)    b = return (Ne (NUnfold n b))
unfold _         _ = throwError "Fold expected"

eval :: (Clos Term) -> Eval Val
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

evalProg :: Clos Prog -> Eval Scope
evalProg ([],s) = return s
evalProg ((Decl _ x _):tel, s) = do (_,s') <- decl x (PrtInfo x False) s Nothing
                                    evalProg (tel,s')
evalProg ((Defn l x t):tel, s) = do i <- getId l x s
                                    defn' i (t,s)
                                    evalProg (tel,s)
