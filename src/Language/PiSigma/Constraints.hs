module Language.PiSigma.Constraints where

import Control.Monad.Reader
import Control.Monad.Error

import Language.PiSigma.Syntax
import Language.PiSigma.Evaluate
import Language.PiSigma.Equality

data Constr = Constr Ne Pat

data Constrs = InCons | Cons [Constr]

consistent :: Eval Bool
consistent = do cs <- ask
                case cs of
                  InCons -> return False
                  Cons _ -> return True

emptyC :: Constrs
emptyC = Cons []

addC :: Clos Term -> Pat -> Eval a -> Eval a
addC t p k = do
  cs <- ask
  case cs of
    InCons -> k
    Cons cs' -> do v <- eval t
                   case v of 
                     VLabel l -> case p of 
                                   PLabel l' | l == l' -> k
                                             | otherwise -> local (const InCons) $ k
                     Ne n -> local (const (Cons (Constr n p : cs'))) $ k
                             -- should check for mutual inconsistency!

ifOk :: Eval a -> Eval r -> Eval r -> Eval r
ifOk c i e = do b <- catchError (c >> return True) (const $ return False)
                if b then i else e

-- invariant: should never be called for inconsistent constraints.
ne :: Ne -> Eval Val
ne n = do Cons cs <- ask
          r <- match cs n
          case r of
            Nothing -> return (Ne n)
            Just p -> return (pat2val p)
      where match :: [Constr] -> Ne -> Eval (Maybe Pat)
            match [] _ = return Nothing
            match (Constr n' p : cs') n =
                  ifOk (eq n n') (return (Just p)) (match cs' n)