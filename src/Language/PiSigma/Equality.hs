{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.PiSigma.Equality
  ( eq )
  where

import Control.Monad

import Language.PiSigma.Evaluate
import Language.PiSigma.Syntax

class Equal a where
    eq :: a -> a -> Eval ()

instance Equal (Clos Term) where
    eq t u = do t' <- eval t
                u' <- eval u
                eq t' u'

-- unused?
-- eq' :: Env e => Clos (Term,Term) -> Eval e ()
-- eq' ((t,u),s) = eq (t,s) (u,s)

eqBind :: (Closure a) =>
          (a -> a -> Eval ()) ->
          Bind a -> Bind a -> Eval ()
eqBind eqArg (x0,c0) (x1,c1) =
  do let s0 = getScope c0
         s1 = getScope c1
     (i,s0') <- decl' x0 s0
     let s1' = extendScope s1 x1 (i,Nothing)
     let c0' = putScope c0 s0'
         c1' = putScope c1 s1'
     eqArg c0' c1'

instance (Equal a,Closure a) => Equal (Bind a) where
    eq = eqBind eq

instance Equal Val where
    eq (Ne t0) (Ne t1) = eq t0 t1
    eq (VQ ps0 ((a0,(x0,b0)),s0)) (VQ ps1 ((a1,(x1,b1)),s1))
      | ps0 == ps1 =
        do eq (a0,s0) (a1,s1)
           eq (x0,(b0,s0)) (x1,(b1,s1))
    eq (VLam xt0) (VLam xt1) = eq xt0 xt1
    eq (VPair ((t0,u0),s0)) (VPair ((t1,u1),s1)) =
        do eq (t0,s0) (t1,s1)
           eq (u0,s0) (u1,s1)
    eq (VBox b) (VBox b') = eq b b'
    eq (VLift a) (VLift a') = eq a a'
    eq (VRec a) (VRec a') = eq a a'
    eq (VFold a) (VFold a') = eq a a'
    eq v0 v1 | v0 == v1 = return () -- Type, Label, Enum
             | otherwise = fail "Different values"

{- eqBox implements alpha equality -}
eqBox :: Clos Term -> Clos Term -> Eval ()
--eqBox c c' | c == c' = return ()
eqBox (Var l x,s) (Var l' y,s') =
    do x' <- getId l  x s
       y' <- getId l' y s'
       eq x' y'
eqBox (Let _ p t,s) c =
    do s' <- evalProg (p,s)
       eqBox (t,s') c
eqBox c c'@(Let _ _ _,_) = eqBox c' c
eqBox (Q _ ps (a,(x,b)),s) (Q _ ps' (a',(x',b')),s')
      | ps == ps' =
          do eqBox (a,s) (a',s')
             eq (x,Boxed (b,s)) (x',Boxed (b',s'))
eqBox (Lam _ (x,t),s) (Lam _ (x',t'),s') =
      eq (x,Boxed (t,s)) (x',Boxed (t',s'))
eqBox (App t u,s) (App t' u',s') =
    do eqBox (t,s) (t',s')
       eqBox (u,s) (u',s')
eqBox (Pair _ t u,s) (Pair _ t' u',s') =
    do eqBox (t,s) (t',s')
       eqBox (u,s) (u',s')
eqBox (Split _ t (x,(y,u)),s) (Split _ t' (x',(y',u')),s') =
    do eqBox (t,s) (t',s')
       eq (x,(y,Boxed (u,s))) (x',(y',Boxed (u',s')))
eqBox (Case _ t bs,s) (Case _ t' bs',s') =
    do eqBox (t,s) (t',s')
       zipWithM_ (\ (l,t'') (l',t''') ->
                      if l==l' then eqBox (t'',s) (t''',s')
                      else fail "eqBox case") bs bs'
eqBox (Lift _ t,s)  (Lift _ t',s')  = eqBox (t,s) (t',s')
eqBox (Box _ t,s)   (Box _ t',s')   = eqBox (t,s) (t',s')
eqBox (Force _ t,s) (Force _ t',s') = eqBox (t,s) (t',s')
eqBox (Rec _ t,s)   (Rec _ t',s')   = eqBox (t,s) (t',s')
eqBox (Fold _ t,s)  (Fold _ t',s')  = eqBox (t,s) (t',s')
eqBox (Unfold _ t (x, u), s) (Unfold _ t' (x', u'), s') =
    do eqBox (t,s) (t',s')
       eq (x,Boxed (u,s)) (x',Boxed (u',s'))
eqBox (t,_) (t',_) | t == t'   = return () -- Type, Label, Enum
                    | otherwise = fail "Different terms"

instance Equal Boxed where
    eq (Boxed c) (Boxed c') = eqBox c c'

instance Equal Id where
    eq i0 i1
        | i0 == i1  = return ()
        | otherwise = do ei0 <- lookupId i0
                         ei1 <- lookupId i1
                         case (ei0,ei1) of
                           (Id j0, Id j1) -> unless (j0 == j1)
                                               (fail "Different variables")
                           (Closure t0, Closure t1) ->
                               letn i0 (Id i0)
                                    (letn i1 (Id i0)
                                          (eq t0 t1))
                           _ -> fail "Variable vs neutral"

instance Equal Ne where
    eq (NVar i0) (NVar i1) = eq i0 i1
    eq (t0 :.. u0) (t1 :.. u1) =
        do eq t0 t1
           eq u0 u1
    eq (NSplit t0 xyu0) (NSplit t1 xyu1) =
        do eq t0 t1
           eq xyu0 xyu1
    eq (NCase t0 (lus0,s0)) (NCase t1 (lus1,s1)) =
        do eq t0 t1
           let eqBranches []             []             = return ()
               eqBranches ((l0,u0):lus0') ((l1,u1):lus1')
                   | l0 == l1                           = do
                      eq (u0,s0) (u1,s1)
                      eqBranches lus0' lus1'
               eqBranches _ _ = fail "Case: branches differ"
           eqBranches lus0 lus1
    eq (NForce t) (NForce t') = eq t t'
    eq (NUnfold t xu) (NUnfold t' xu') = do
      eq t t'
      eq xu xu'
    eq t u = fail ("Different neutrals:\n"++ show t ++"\n/=\n"++ show u ++"\n")
