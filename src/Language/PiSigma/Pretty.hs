{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.PiSigma.Pretty
  ( module Text.PrettyPrint.MPPPC.OneDim
  , Pretty
  , Print
  , evalPrint
  , fromPretty )
  where

import Control.Monad.Error
import Text.PrettyPrint.MPPPC.OneDim
  hiding
    ( Pretty )
import qualified Text.PrettyPrint.MPPPC.OneDim
  as OneDim

import Language.PiSigma.Evaluate
import Language.PiSigma.Normalise
import Language.PiSigma.Syntax
import qualified Language.PiSigma.Util.String.Internal
  as Internal

type Pretty = OneDim.Pretty (Seq Internal.String Char) (Tok Internal.String Char)

-- * Print class for various types

class Print a where
    evalPrint :: a -> Eval Pretty

instance Print Internal.String where
    evalPrint   = return . text . Seq

instance Print Pretty where
    evalPrint   = return

instance Print Val where
    evalPrint a = evalPrint =<< quote [] a

instance Print (Clos Term) where
    evalPrint a = evalPrint =<< quote [] a

instance Print Ne where
    evalPrint a = evalPrint =<< quote [] a

instance Print Term where
    evalPrint   = return . prettyTerm 0

fromPretty :: Pretty -> Internal.String
fromPretty = unSeq . renderSeq

data Msg
  = MsgV Val
  | MsgC (Clos Term)
  | MsgT Term
  | MsgS Internal.String
  | MsgD Pretty

instance Print Msg where
    evalPrint (MsgV v) = evalPrint v
    evalPrint (MsgC c) = evalPrint c
    evalPrint (MsgT t) = evalPrint t
    evalPrint (MsgS s) = evalPrint s
    evalPrint (MsgD d) = evalPrint d

-- * Pretty printer for terms

-- | Context for printing. Determines whether parentheses are
-- required.
type PrintContext = Int


prettyTerm :: PrintContext -> Term -> Pretty

prettyTerm _ (Var _ x)                   =
      text $ Seq x

prettyTerm c (Let _ p t)                 =
      contextParens c 0
   $  text "let"
  <+> sep (map prettyEntry p)
  <+> text "in"
  <+> prettyTerm 0 t

prettyTerm _ (Type _)                    =
      text "Type"

prettyTerm c (Q _ Pi (t1, (n, t2)))      =
      contextParens c 0
   $  group
   $  binding 1 n t1
  <$> text "->"
  <+> prettyTerm 0 t2

prettyTerm c (Q _ Sigma (t1, (n, t2)))   =
      contextParens c 0
   $  binding 1 n t1
  <+> text "*"
  <+> prettyTerm 0 t2

prettyTerm c (Lam _ (n, t))              =
      contextParens c 0
   $  text "\\"
  <+> text (Seq n)
  <+> text "->"
  <+> prettyTerm 0 t

prettyTerm c (App t1 t2)                 =
      group
   $  hang 2
   $  contextParens c 1
   $  prettyTerm 1 t1
  <$> prettyTerm 2 t2

prettyTerm _ (Pair _ t1 t2)              =
      tupled $ map (prettyTerm 0) [t1, t2]

prettyTerm c (Split _ t1 (n1, (n2, t2))) =
      contextParens c 0
   $  hang 2
   $  text "split"
  <+> prettyTerm 0 t1
  <+> text "with"
  <+> (parens $  text (Seq n1)
              <> comma
              <> text (Seq n2))
  <+> text "->"
  <$> prettyTerm 0 t2

prettyTerm _ (Enum _ ls)                 =
      braces
   $  sep
   $  map (text . Seq) ls

prettyTerm _ (Label _ l)                 =
      text $ Tok '\'' `cons` Seq l

prettyTerm _ (Case _ t bs)               =
      hang 1
   $  text "case"
  <+> prettyTerm 0 t
  <+> text "of"
  <$> branches bs

prettyTerm c (Lift _ t)                  =
      contextParens c 1
   $  text "^"
  <+> prettyTerm 2 t

prettyTerm _ (Box _ t)                   =
      brackets $ prettyTerm 0 t

prettyTerm c (Force _ t)                 =
      contextParens c 1
   $  text "!"
  <+> prettyTerm 2 t

prettyTerm c (Rec _ t)                  =
     contextParens c 1
   $ text "Rec"
  <+> prettyTerm 2 t

prettyTerm c (Fold _ t)                  =
     contextParens c 1
   $ text "fold"
  <+> prettyTerm 2 t

prettyTerm c (Unfold _ t1 (n, t2))       =
      contextParens c 0
   $  hang 2
   $  text "unfold"
  <+> prettyTerm 0 t1
  <+> text "as"
  <+> text (Seq n)
  <+> text "->"
  <$> prettyTerm 0 t2


prettyEntry :: Entry -> Pretty

prettyEntry (Defn _ n t)                 =
      hang 2
   $  text (Seq n)
  <+> text "="
  <+>  prettyTerm 0 t
  <>  text ";"

prettyEntry (Decl _ n t)                 =
      hang 2
   $  text (Seq n)
  <+> text ":"
  <>  prettyTerm 0 t
  <>  text ";"


binding :: PrintContext -> Name -> Term -> Pretty
binding c n t | Internal.null n =  prettyTerm c t
              | otherwise       =  parens
                                $  text (Seq n)
                               <+> colon
                               <+> prettyTerm 0 t

branches :: [(Label,Term)] -> Pretty
branches = encloseSep (text " { ") (text " } ") (text " | ") . map branch
  where
    branch (n, t) =  text (Seq n)
                 <+> text "->"
                 <+> prettyTerm 0 t

-- | Prints parens if the current context is higher
-- than a certain limit.
contextParens :: PrintContext -> PrintContext -> Pretty -> Pretty
contextParens c d p | c > d     = parens p
                    | otherwise = p
