{-# LANGUAGE OverloadedStrings #-}

module Language.PiSigma.Check
  ( checkProg
  , infer )
  where

import Control.Arrow
  ( first )
import Control.Monad.Error
import qualified Data.List
  as List

import Language.PiSigma.Equality
import Language.PiSigma.Evaluate
import Language.PiSigma.Pretty
import Language.PiSigma.Syntax
import qualified Language.PiSigma.Util.String.Internal
  as Internal
import qualified Data.Set as Set

--import Debug.Trace
--trace "check\n" $

-- closures are the other way around
-- should be fixed by changing closures.

{-
fog :: CTerm -> Closure
fog (g,t) = (con2scope g,t)
-}

throwErrorc :: (Print b, GetLoc b, Env e) => b -> Pretty -> Eval e a
throwErrorc t m =
  do
    pt <- evalPrint t
    throwError $ fromPretty $
             text (Seq $ Internal.fromString $ locMessage $ getLoc t)
         <$> "Expression:   " <+> align pt
         <$> m

expected :: (Env e) => Clos Term -> Val -> Pretty -> Eval e ()
expected t expected' inferred = do
  pExpected <- evalPrint expected'
  throwErrorc t $ "Expected type:" <+> align pExpected <$> parens inferred

expectedButFound :: (GetLoc a, Print a, Print b, Print c, Env e)
                 => a
                 -> b
                 -> c
                 -> Internal.String
                 -> Eval e d
expectedButFound t expected' found inferred =
  do
    pExpected <- evalPrint expected'
    pFound    <- evalPrint found
    throwErrorc t $
          "Inferred type:" <+> align pFound
      <$> "Expected type:" <+> align pExpected
      <$> parens (text . Seq $ inferred)

duplicateLabels :: (GetLoc a, Print a, Env e) => a -> Eval e d
duplicateLabels t =
  throwErrorc t $ "Duplicate labels in enum type"

nonLinearSplit :: (GetLoc a, Print a, Env e) => a -> Eval e d
nonLinearSplit t =
  throwErrorc t $ "Repeated variables in a split"

-- | Takes a term and an (unevaluated) type. We first
-- handle the cases that may potentially change the
-- environment. If none of those cases match, we can
-- safely evaluate the type and call check'.
check :: Env e => Clos Term -> Clos Type -> Eval e ()
--check (g,t) c | trace ("check\ng ="++(show g)++"\n t="++(show t)++"\nc="++(show c)++"\n") False = undefined

check (Let _ p t,g) c =
    do g' <- checkProg (p,g)
       check (t,g') c

check (Split _ t (x,(y,u)),g) c | x==y = nonLinearSplit t
check (Split _ t (x,(y,u)),g) c | otherwise =
    do sigab <- infer' (t,g)
       case sigab of
         (VQ Sigma ((a,(z,b)),s)) ->
             do t'      <- eval (t,g)
                (_,g')  <- tdecl x g (a,s)
                b'      <- subst (z,(b,s)) (Var Unknown x, g')
                (_,g'') <- tdecl y g' b'
                case t' of
                  (Ne (NVar i)) ->
                      letn' i (Pair Unknown (Var Unknown x) (Var Unknown y), g'')
                                (check (u,g'') c)
                      -- this is a bit silly since we know the ids.
                  _ -> check (u,g'') c
                      -- instead of failing, we just omit the assignment.
         _ -> expectedButFound (t,g) msg1 sigab msg2
                where
                  msg1 = "sigma type" :: Internal.String
                  msg2 = "split"      :: Internal.String

check (Unfold _ t (x,u),g) c =
    do rec <- infer' (t,g)
       case rec of
         VRec (a,s) ->
             do t'      <- eval (t,g)
                (_,g')  <- tdecl x g (Force Unknown a,s)
                case t' of
                  (Ne (NVar i)) ->
                      letn' i (Fold Unknown (Var Unknown x), g')
                            (check (u,g') c)
                  _ -> check (u,g') c
         _ -> expectedButFound (t,g) msg1 rec msg2
                where
                  msg1 = "rec type" :: Internal.String
                  msg2 = "unfold"   :: Internal.String

check gt @ (Case _ t lus,g) c =
    do enum <- infer' (t,g)
       case enum of
         VEnum ls ->
             let ls' = map fst lus
             in if ls /= ls'
                then -- pt' <- evalPrint t'
                  throwErrorc gt $ text $ Seq $
                    Internal.concat [ "Labels don't match."
                                    , "\nProvided labels: ", Internal.fromString $ show ls
                                    , "\nExpected labels: ", Internal.fromString $ show ls'
                                    , "\n(Case)\n"
                                    ]
                -- set equivalence would be sufficent
                else do t' <- eval (t,g)
                        case t' of
                          -- if the scrutinee is a variable, we add a constraint
                          -- while checking each of the branches
                          (Ne (NVar i)) ->
                               mapM_ (\ (l,u) ->
                                      letn' i (label l)
                                            (check (u,g) c)) lus
                          -- if the scrutinee is not a variable, we do not add
                          -- a constraint, but continue
                          _ -> mapM_ (\ (_,u) -> check (u,g) c) lus
         _ -> expectedButFound (t,g) msg1 enum msg2
                where
                  msg1 = "enum type" :: Internal.String
                  msg2 = "case"      :: Internal.String


         {- Problem: here and other places: we try to print the term t which isn't yet
            typechecked and may contain undefined variables which may crash the printer...
            see undef-bug.pi
         -}
check (Force _ t,g) c = check (t,g) (tlift c)

check t a = check' t =<< eval a

check' :: Env e => Clos Term -> Val -> Eval e ()
check' (Lam _ (x,t),g) (VQ Pi ((a,(y,b)),s)) =
    do (i,g') <- tdecl x g (a,s)
       let s' = extendScope s y (i,Nothing)
       check (t,g') (b,s')
check' gt @ (Lam _ _,_) a =
    expected gt a "Lam"
check' (Pair _ t u,g) (VQ Sigma ((a,(y,b)),s)) =
    do check (t,g) (a,s)
       b' <- subst (y,(b,s)) (t,g)
       check (u,g) b'
check' gt @ (Pair _ _ _, _) a =
    expected gt a "Pair"
-- Labels cannot be inferred because there is no way to know
-- what the other labels are.
check' (Label _ l,_) (VEnum ls) | l `elem` ls = return ()
check' gt @ (Label _ _,_) a =
    expected gt a "Label"
check' (Box _ t,g) (VLift a) =
    check (t,g) a
check' (Fold _ t,g) (VRec a) =
    check' (t,g) =<< eval (tforce a)

check' t a =
    do b <- infer' t
       catchE (eq a b) (\ s -> expectedButFound t a b (Internal.append "check: " s))

inferVar :: Env e => Loc -> Clos Name -> Eval e (Clos Type)
inferVar l (x,g) =
    case lookupCon g x of
      Just a  -> return a
      Nothing -> throwError msg
        where
          msg = Internal.concat [ Internal.fromString $ locMessage l
                                , "\nUndefined variable: "
                                , x
                                , "\n(inferVar)\n"
                                ]

infer :: Env e => Clos Term -> Eval e (Clos Type)
--infer (g,t) | trace ("infer\ng ="++(show g)++"\n t="++(show t)++"\n") False = undefined

infer (Var l x,g) = inferVar l (x,g)

infer (Let _ tel t,g) =
    do g' <- checkProg (tel,g)
       infer (t,g')

infer (Type _,_) = return ty

infer (Q _ _ (a,(x,b)),g) =
    do check (a,g) ty
       (_,g') <- tdecl x g (a,g)
       check (b,g') ty
       return ty

infer (App t u,g) =
    do piab <- infer' (t,g)
       case piab of
         (VQ Pi ((a,(x,b)),s)) -> do check (u,g) (a,s)
                                     subst (x,(b,s)) (u,g)
         _ -> expectedButFound (t,g) msg1 piab msg2
                where
                  msg1 = "pi type" :: Internal.String
                  msg2 = "App"     :: Internal.String

infer (t@(Enum _ ls),_) =
    if List.length (List.nub ls) < List.length ls
      then duplicateLabels t
      else return ty

infer (Box  _ t,g) = liftM (first $ Lift Unknown) (infer (t,g))

infer (Fold  _ t,g) =
  liftM (first $ Rec Unknown . Box Unknown) (infer (t,g))

infer (Force _ t,g) =
    do a <- infer' (t,g)
       case a of
         VLift b -> return b
         _       -> expectedButFound (t,g) msg1 a msg2
                      where
                        msg1 = "lifted type" :: Internal.String
                        msg2 = "Force"       :: Internal.String

infer (Lift _ a,g) =
    do check (a,g) ty
       return ty

infer (Rec _ a,g) =
    do check (a,g) lty
       return ty

infer gt = throwErrorc gt $ "Not inferable" <$> "(infer)"


-- | Infers a type and evaluates it.
infer' :: Env e => Clos Term -> Eval e Val
infer' gt = eval =<< infer gt

checkProg :: Env e => Clos Prog -> Eval e Scope
checkProg st = checkProg' Set.empty Set.empty st

declaredButNoteDefined :: Set.Set Name -> Eval e Scope
declaredButNoteDefined xs = throwError ("Variables declared but not defined")

declaredTwice :: Name -> Eval e Scope
declaredTwice x = throwError ("Variable declared twice in the same block.")

notDeclHere :: Name -> Eval e Scope
notDeclHere x = throwError "Name not declared in the same context"


-- to be finished
checkProg' :: Env e => Set.Set Name -> Set.Set Name -> Clos Prog -> Eval e Scope
checkProg' decls defns ([],g) = return g
--    do
--      let declndef = decls `Set.difference` defns
--      if not (Set.null declndef)
--       then declaredButNoteDefined declndef
--       else return g
checkProg' decls defns ((Decl _ x a):tel,g) =
    do check (a,g) ty
       (_,g') <- decl x PrtInfo{ name = x, expand = False } g (Just (a,g))
       if x `Set.member` decls
        then declaredTwice x
        else checkProg' (Set.insert x decls) defns (tel,g')
checkProg' decls defns ((Defn l x t):tel,g) =
    do a <- inferVar l (x,g)
       check (t,g) a
       i <- getId l x g
       defn' i (t,g)
       if not (x `Set.member` decls) 
        then notDeclHere x
        else checkProg' decls (Set.insert x defns) (tel,g)


