{-

The following grammar is (supposed to be) implemented below:

variable ∷= …

label ∷= …

arrow ∷= "→" | "->"

lambda ∷= "λ" | "\\"

branch ∷= label arrow term

branch⋆
 ∷= branch "|" branch⋆
  |

label⋆
 ∷= label label⋆
  |

variable⁺
 ∷= variable variable⁺
  | variable

prefix-operator
 ∷= "Rec" | "fold" | "unfold" | "^" | "∞" | "!" | "♭" | "♯"

infix-operator ∷= arrow | "*"

entry
 ∷= variable ":" term ( | "=" term)
  | variable "=" term

entry⁺
 ∷= entry ";" entry⁺
  | entry

program
 ∷= entry⁺ ";"
  | entry⁺
  |

atom
 ∷= "(" term ")"
  | variable
  | "Type"
  | "(" term "," term ")"
  | "{" label⋆ "}"
  | "′" label
  | "case" term "of" "{" branch⋆ "}"
  | "[" term "]"

atom-or-application
 ∷= atom
  | prefix-operator atom
  | atom-or-application atom

term
  ∷= atom-or-application ( | infix-operator term)
   | "let" program "in" term
   | "(" variable⁺ ":" term ")" infix-operator term
   | lambda variable⁺ arrow term
   | "split" term "with" "(" variable "," variable ")" arrow term
   | "unfold" term "as" variable arrow term

-}

module Language.PiSigma.Parser
  ( parse
  , sPhrase
  , sProg
  , s2Terms
  , sTerm )
  where

import Prelude hiding (pi)
import Control.Monad
import Control.Applicative hiding ((<|>), many)
import Data.List
import Text.Parsec.Combinator
import Text.Parsec.Error
  ( ParseError )
import Text.Parsec.Pos
  ( SourceName )
import Text.Parsec.Prim
  hiding
    ( label
    , parse
    , token )
import qualified Text.ParserCombinators.Parsec
  as Parsec
    ( parse )
import Text.ParserCombinators.Parsec.Char
  ( string )

import Language.PiSigma.Lexer
import Language.PiSigma.Syntax hiding (label)
import qualified Language.PiSigma.Util.String.Internal
  as Internal
import qualified Language.PiSigma.Util.String.Parser
  as Parser

sLabel :: Parser Name
sLabel  = Internal.fromString <$> (string "'" *> identifier)

sName  :: Parser Name
sName   = Internal.fromString <$> identifier

-- * Terms

prefixOperator :: Parser (Term -> Term)
prefixOperator = choice
  [ Rec    <$> locReserved "Rec"
  , Fold   <$> locReserved "fold"
  , Lift   <$> tokLift
  , Force  <$> tokForce
  , Box    <$> locReservedOp "♯"
  , unfold <$> locReserved "unfold"
  ]
  where
  unfold l t = Unfold l t id
  id         = ( Internal.fromString " x"
               , Var Unknown (Internal.fromString " x")
               )

infixOperator :: Parser (Term -> Term -> Term)
infixOperator = choice
  [ (->-) <$ tokArr
  , (-*-) <$ locReservedOp "*"
  ]

atom :: Parser Term
atom = choice
  [ try $ parens sTerm

  , Type  <$> locReserved "Type"

  , pair  <$>     location
              <*> parens ((,) <$> sTerm <* comma <*> sTerm)
          <?> "pair"

  , Enum  <$>     location
              <*> braces (many sName)
          <?> "enumeration"

  , Label <$>     location
              <*> sLabel
          <?> "label"

  , Case  <$>     locReserved "case"
              <*> sTerm
              <*  reserved    "of"
              <*> braces (sBranch `sepBy` locReservedOp "|")

  , Box   <$>     location
              <*> brackets sTerm
          <?> "box"

  , Var   <$> location <*> sName
  ]
  where pair = uncurry . Pair

atomOrApplication :: Parser Term
atomOrApplication =
  foldl App <$> (atom <|> prefixOperator <*> atom) <*> many atom

sTerm :: Parser Term
sTerm = choice
  [ Let   <$> locReserved "let"
          <*> sProg
          <*     reserved "in"
          <*> sTerm

  , lam   <$  tokLam
          <*> many1 ((,) <$> location <*> sName)
          <*  tokArr
          <*> sTerm

  , split <$> locReserved "split"
          <*> sTerm
          <*     reserved "with"
          <*> parens ((,) <$> sName
                          <*  comma
                          <*> sName)
          <*  tokArr
          <*> sTerm

  , try (Unfold <$> locReserved "unfold"
                <*> sTerm
                <*  reserved "as")
                <*> ((,) <$> sName
                         <*  tokArr
                         <*> sTerm)

  , try (do (ns, t) <- parens $
              (,) <$> many1 ((,) <$> location <*> sName)
                  <*  reservedOp ":"
                  <*> sTerm
            op <- choice [ pis    <$ tokArr
                         , sigmas <$ reservedOp "*"
                         ]
            return $ op ns t) <*> sTerm

  , (\a -> maybe a ($ a)) <$>
      atomOrApplication <*>
      optionMaybe (flip <$> infixOperator <*> sTerm)
  ]

sProg :: Parser Prog
sProg = concat <$> (sEntry `sepEndBy` semi)

sEntry :: Parser [Entry]
sEntry =
    do
      l <- location
      n <- sName
      b <- choice [ True  <$ reservedOp ":"
                  , False <$ reservedOp "="
                  ]
      t <- sTerm
      if b
        then do
               d <- option Nothing (Just <$ reservedOp "=" <*> sTerm)
               case d of
                 Nothing -> return [Decl l n t]
                 Just t' -> return [Decl l n t, Defn l n t']
        else return [Defn l n t]

sBranch :: Parser (Label,Term)
sBranch = (,) <$> sName <* tokArr <*> sTerm

sPhrase :: Parser Phrase
sPhrase = choice
  [ try $ Prog <$> sProg <* eof
  ,       Term <$> sTerm <* eof
  ]

s2Terms :: Parser (Term, Term)
s2Terms = (,) <$> atom <*> atom

parse :: Parser a -> SourceName -> Parser.String -> Either ParseError a
parse p = Parsec.parse (whiteSpace *> p <* eof)
