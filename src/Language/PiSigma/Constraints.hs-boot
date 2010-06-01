module Language.PiSigma.Constraints where

import Language.PiSigma.Syntax

data Constr = Constr Ne Pat

data Constrs = InCons | Cons [Constr]

emptyC :: Constrs

