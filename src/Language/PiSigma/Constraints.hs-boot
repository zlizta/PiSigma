module Language.PiSigma.Constraints where

import Language.PiSigma.Syntax
import Language.PiSigma.Eval

addC :: Clos Term -> Pat -> Eval a -> Eval a

ne :: Ne -> Eval Val