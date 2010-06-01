{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
module Language.PiSigma.Eval where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Reader

import Language.PiSigma.Syntax
import qualified Language.PiSigma.Util.String.Internal as Internal

-- * Monad used for evaluation

type EvalErr = Internal.String

instance Error EvalErr where


newtype Eval a = Eval { unEval :: ReaderT Constrs (StateT EnvEntries (ErrorT EvalErr Identity)) a }
  deriving ( Monad
           , MonadError EvalErr
           , MonadState EnvEntries
           , MonadReader Constrs)

run :: EnvEntries -> Eval a -> Either EvalErr (a, EnvEntries)
run e (Eval p) = runIdentity $ runErrorT $ runStateT (runReaderT p emptyC) e 

catchE :: Eval a -> (EvalErr -> Eval a) -> Eval a
catchE = catchError

getEnv :: Eval EnvEntries
getEnv = get

setEnv :: EnvEntries -> Eval ()
setEnv = put

