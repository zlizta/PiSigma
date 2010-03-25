{-# OPTIONS_GHC -fno-warn-orphans     #-}
{-# LANGUAGE    FlexibleInstances     #-}
{-# LANGUAGE    MultiParamTypeClasses #-}
{-# LANGUAGE    TypeSynonymInstances  #-}

module Language.PiSigma.Lexer
  ( Parser
  , angles
  , braces
  , brackets
  , charLiteral
  , colon
  , comma
  , commaSep
  , commaSep1
  , decimal
  , dot
  , float
  , hexadecimal
  , identifier
  , integer
  , locate
  , location
  , locReserved
  , locReservedOp
  , locSymbol
  , lexeme
  , natural
  , naturalOrFloat
  , octal
  , operator
  , parens
  , reserved
  , reservedOp
  , semi
  , semiSep
  , semiSep1
  , squares
  , stringLiteral
  , symbol
  , tokArr
  , tokForce
  , tokLam
  , tokLift
  , whiteSpace )
  where

import Control.Applicative
import Control.Monad.Identity
import Data.Char
import Text.Parsec.Prim
  ( Parsec
  , Stream (..)
  , (<?>)
  , getPosition
  )
import qualified Text.Parsec.Token
  as Token
import Text.ParserCombinators.Parsec
  ( SourcePos
  , choice
  , sourceColumn
  , sourceLine
  , sourceName )
import Text.ParserCombinators.Parsec.Char

import Language.PiSigma.Syntax
  ( Loc (..) )
import qualified Language.PiSigma.Util.String.Parser
  as Parser

instance (Monad m) => Stream Parser.String m Char where
  uncons = return . Parser.uncons

type Parser = Parsec Parser.String ()

nonIdentStr :: String
nonIdentStr = [ '('
              , ')'
              , '['
              , ']'
              , '{'
              , '}' ]

opLetterStr :: String
opLetterStr = [ '!'
              , '*'
              , ','
              , '-'
              , ':'
              , ';'
              , '='
              , '>'
              , '\\'
              , '^'
              , '|'
              , '♭'
              , '♯'
              , 'λ'
              , '→'
              , '∞' ]

-- * The lexical definition of PiSigma.  Used to make token parsers.
pisigmaDef :: (Monad m) => Token.GenLanguageDef Parser.String u m
pisigmaDef = Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = satisfy $ \ c -> not (isSpace   c)
                                          && not (c `elem` nonIdentStr)
                                          && not (c `elem` opLetterStr)
                                          && not (isControl c)
                                          && not (isDigit   c)
  , Token.identLetter     = satisfy $ \ c -> not (isSpace   c)
                                          && not (c `elem` nonIdentStr)
                                          && not (c `elem` opLetterStr)
                                          && not (isControl c)
  , Token.opStart         = oneOf ""
  , Token.opLetter        = oneOf opLetterStr
  , Token.reservedNames   = [ "Type"
                            , "case"
                            , "in"
                            , "let"
                            , "of"
                            , "split"
                            , "with"
                            , "Rec"
                            , "fold"
                            , "unfold"
                            , "as"]
  , Token.reservedOpNames = [ "!"
                            , "*"
                            , ","
                            , "->"
                            , ":"
                            , ";"
                            , "="
                            , "\\"
                            , "^"
                            , "|"
                            , "♭"
                            , "♯"
                            , "λ"
                            , "→"
                            , "∞" ]
  , Token.caseSensitive   = True
  }

-- * The PiSigma token parser, generated from the lexical definition.

tokenParser :: Token.GenTokenParser Parser.String () Identity
tokenParser =  Token.makeTokenParser pisigmaDef

-- * PiSigma parser combinators.

angles           ::  Parser a -> Parser a
angles            =  Token.angles         tokenParser

braces           ::  Parser a -> Parser a
braces            =  Token.braces         tokenParser

brackets         ::  Parser a -> Parser a
brackets          =  Token.brackets       tokenParser

charLiteral      ::  Parser Char
charLiteral       =  Token.charLiteral    tokenParser

colon            ::  Parser String
colon             =  Token.colon          tokenParser

comma            ::  Parser String
comma             =  Token.comma          tokenParser

commaSep         ::  Parser a -> Parser [a]
commaSep          =  Token.commaSep       tokenParser

commaSep1        ::  Parser a -> Parser [a]
commaSep1         =  Token.commaSep1      tokenParser

decimal          ::  Parser Integer
decimal           =  Token.decimal        tokenParser

dot              ::  Parser String
dot               =  Token.dot            tokenParser

float            ::  Parser Double
float             =  Token.float          tokenParser

hexadecimal      ::  Parser Integer
hexadecimal       =  Token.hexadecimal    tokenParser

identifier       ::  Parser String
identifier        =  Token.identifier     tokenParser

integer          ::  Parser Integer
integer           =  Token.integer        tokenParser

lexeme           ::  Parser a -> Parser a
lexeme            =  Token.lexeme         tokenParser

natural          ::  Parser Integer
natural           =  Token.natural        tokenParser

naturalOrFloat   ::  Parser (Either Integer Double)
naturalOrFloat    =  Token.naturalOrFloat tokenParser

octal            ::  Parser Integer
octal             =  Token.octal          tokenParser

operator         ::  Parser String
operator          =  Token.operator       tokenParser

parens           ::  Parser a -> Parser a
parens            =  Token.parens         tokenParser

reserved         ::  String -> Parser ()
reserved          =  Token.reserved       tokenParser

reservedOp       ::  String -> Parser ()
reservedOp        =  Token.reservedOp     tokenParser

semi             ::  Parser String
semi              =  Token.semi           tokenParser

semiSep          ::  Parser a -> Parser [a]
semiSep           =  Token.semiSep        tokenParser

semiSep1         ::  Parser a -> Parser [a]
semiSep1          =  Token.semiSep1       tokenParser

squares          ::  Parser a -> Parser a
squares           =  Token.squares        tokenParser

stringLiteral    ::  Parser String
stringLiteral     =  Token.stringLiteral  tokenParser

symbol           ::  String -> Parser String
symbol            =  Token.symbol         tokenParser

whiteSpace       ::  Parser ()
whiteSpace        =  Token.whiteSpace     tokenParser

-- * Derived parser combinators

location         :: Parser Loc
location          = sourcePosToLoc <$> getPosition

locate           :: Parser a -> Parser Loc
locate            = (location <*)

sourcePosToLoc   :: SourcePos -> Loc
sourcePosToLoc p  = Loc (sourceName p) (sourceLine p) (sourceColumn p)

locReserved      :: String -> Parser Loc
locReserved       = locate . reserved

locReservedOp    :: String -> Parser Loc
locReservedOp     = locate . reservedOp

locSymbol        :: String -> Parser Loc
locSymbol xs      = locate (symbol xs) <?> show xs

tokArr           :: Parser Loc
tokArr            = locate (choice [ reservedOp "->"
                                   , reservedOp "→"
                                   ] <?> "->")

tokForce         :: Parser Loc
tokForce          = locate (choice [ reservedOp "!"
                                   , reservedOp "♭"
                                   ] <?> "!")

tokLam           :: Parser Loc
tokLam            = locate (choice [ reservedOp "\\"
                                   , reservedOp "λ"
                                   ] <?> "\\")

tokLift          :: Parser Loc
tokLift           = locate (choice [ reservedOp "^"
                                   , reservedOp "∞"
                                   ] <?> "^")
