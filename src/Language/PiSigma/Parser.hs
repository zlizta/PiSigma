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

sTerm5 :: Parser Term
sTerm5 = choice
  [ Type  <$> locReserved "Type"

  , Enum  <$>     location
              <*> braces (many sName)
          <?> "enumeration"

  , Case  <$>     locReserved "case"
              <*> sTerm
              <*  reserved    "of"
              <*> braces (sBranch `sepBy` locReservedOp "|")

  , Label <$>     location
              <*> sLabel
          <?> "label"

  , Box   <$>     location
              <*> boxed sTerm
          <?> "box"

  , Lift  <$>     tokLift
              <*> sTerm5
          <?> "'^'"

  , Rec   <$> locReserved "Rec"
          <*> sTerm

  , Fold  <$> locReserved "fold"
          <*> sTerm

  , Var   <$> location <*> sName
  , parens sTerm
  ]
  where
    boxed p = choice [ brackets          p
                     , reservedOp "♯" *> p
                     ]

sTerm4 :: Parser Term
sTerm4 = choice
  [ try $ (uncurry . Pair) <$> location
                           <*> parens ((,) <$> sTerm <* comma <*> sTerm)
  , sTerm5
  ]

sTerm3 :: Parser Term
sTerm3 = choice
  [ try (parens (sigmas <$> many1 ((,) <$> location <*> sName) <* reservedOp ":" <*> sTerm) <* reservedOp "*") <*> sTerm2
  ,        Force <$> tokForce <*> sTerm4
  , foldl1 App   <$> many1 sTerm4
  ]

sTerm2 :: Parser Term
sTerm2 = foldr1 (-*-) <$> sTerm3 `sepBy1` reservedOp "*"

-- TODO: make more beautiful or renumber
sTerm1b :: Parser Term
sTerm1b = choice
  [ try (parens (pis <$> many1 ((,) <$> location <*> sName) <* reservedOp ":" <*> sTerm) <* tokArr) <*> sTerm
  , sTerm2
  ]

sTerm1 :: Parser Term
sTerm1 = foldr1 (->-) <$> sTerm1b `sepBy1` tokArr

sTerm :: Parser Term
sTerm = choice
  [ lam   <$  tokLam
          <*> many ((,) <$> location <*> sName)
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

  , Unfold <$> locReserved "unfold"
           <*> sTerm
           <*> option
                 (Internal.fromString " x", Var Unknown (Internal.fromString " x"))
                 (reserved "as"
                  *> ((,) <$> sName
                          <*  tokArr
                          <*> sTerm))

  , Let   <$> locReserved "let"
          <*> sProg
          <*     reserved "in"
          <*> sTerm
  , sTerm1
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
s2Terms = (,) <$> sTerm5 <*> sTerm5

parse :: Parser a -> SourceName -> Parser.String -> Either ParseError a
parse p = Parsec.parse (whiteSpace *> p <* eof)
