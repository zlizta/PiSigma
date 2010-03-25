{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.PiSigma.Syntax
  ( Bind
  , Boxed    (..)
  , Clos
  , Closure  (..)
  , Entry    (..)
  , Enum
  , Env
  , EnvEntry (..)
  , EnvEntries
  , GetLoc   (..)
  , Id
  , Label
  , Loc      (..)
  , Name
  , Ne       (..)
  , Phrase   (..)
  , PiSigma  (..)
  , Prog
  , PrtInfo  (..)
  , Scope    (..)
  , Term     (..)
  , Type
  , Val      (..)
  , (-*-)
  , (->-)
  , emptyE
  , emptyScope
  , extE
  , extendScope
  , getE
  , label
  , lam
  , locMessage
  , lookupCon
  , lookupScope
  , pis
  , prtE
  , setE
  , sigmas
  , split
  , ty
  , lty
  , tforce
  , tlift )
  where

import Prelude
  hiding
    ( pi )
import Data.Maybe
  ( fromJust )

import qualified Language.PiSigma.Util.String.Internal
  as Internal

-- * Locations

data Loc
  = Unknown
  | Loc { filename :: !String
        , line     :: !Int
        , column   :: !Int }
  deriving Show

locMessage :: Loc -> String
locMessage Unknown     = "<unknown>:"
locMessage (Loc f l c) = concat
                       [ f
                       , ":"
                       , show l
                       , ":"
                       , show c
                       , ":"
                       ]

-- | Locations are always equal. This allows us to
-- derive equality for the abstract syntax that ignores
-- the locations.
instance Eq Loc where
  _ == _ = True

class GetLoc a where
  getLoc :: a -> Loc

-- * Abstract syntax

data Phrase
  = Prog Prog
  | Term Term
  deriving (Show, Eq)

type Name  = Internal.String

type Label = Internal.String

data Entry
  = Decl Loc Name Type
  | Defn Loc Name Term
  deriving (Show, Eq)

instance GetLoc Entry where
  getLoc (Decl l _ _) = l
  getLoc (Defn l _ _) = l

type Prog = [Entry]

{-
Maybe better
data Prog = Decl Type (Bind Prog) | Defn Name Term Prog
-}

type Type = Term

-- | The use of Bind indicates the scope of the
-- bound identifier.
type Bind a = (Name, a)

-- | For treating quantifiers in a uniform way.
data PiSigma
  = Pi
  | Sigma
  deriving (Show, Eq)

data Term
  = Var   Loc Name
  | Let   Loc Prog Term
  | Type  Loc
  | Q     Loc PiSigma (Type, Bind Type)
  | Lam   Loc (Bind Term)
  | App   Term Term
  | Pair  Loc Term Term
  | Split Loc Term (Bind (Bind Term)) -- split t with (x,y) -> u
  | Enum  Loc [Name]
  | Label Loc Label
  | Case  Loc Term [(Label, Term)]     -- case t of { L -> u }
  | Lift  Loc Term
  | Box   Loc Term
  | Force Loc Term
  | Rec   Loc Term
  | Fold  Loc Term
  | Unfold Loc Term (Bind Term)       -- unfold t as x -> u
  deriving (Show,Eq)

instance GetLoc Term where
  getLoc (Var   l _  ) = l
  getLoc (Let   l _ _) = l
  getLoc (Type  l    ) = l
  getLoc (Q     l _ _) = l
  getLoc (Lam   l _  ) = l
  getLoc (App   t _  ) = getLoc t
  getLoc (Pair  l _ _) = l
  getLoc (Split l _ _) = l
  getLoc (Enum  l _  ) = l
  getLoc (Label l _  ) = l
  getLoc (Case  l _ _) = l
  getLoc (Lift  l _  ) = l
  getLoc (Box   l _  ) = l
  getLoc (Force l _  ) = l
  getLoc (Rec   l _  ) = l
  getLoc (Fold  l _  ) = l
  getLoc (Unfold l _ _) = l

-- * Smart constructors

-- | Smart constructor for lambda abstractions.
lam :: [(Loc, Name)] -> Term -> Term
lam []            t = t
lam ((l, x) : xs) t = Lam l (x, lam xs t)

-- | Smart constructor for split.
split :: Loc -> Term -> (Name, Name) -> Term -> Term
split l t1 (x, y) t2 = Split l t1 (x, (y, t2))

-- | Smart constructor for quantifiers.
q :: PiSigma -> [(Loc, Name, Type)] -> Type -> Type
q _  []                b = b
q ps ((l, x, a) : xas) b = Q l ps (a, (x, q ps xas b))

-- | Smart constructor for multiple Pi applications.
pis' :: [(Loc, Name, Type)] -> Type -> Type
pis'     = q Pi

-- | Smart constructor for multiple Pi applications.
pis :: [(Loc, Name)] -> Type -> Type -> Type
pis ns t = pis' (map (\ (l, n) -> (l, n, t)) ns)

-- | Smart constructor for Pi.
pi :: Loc -> Name -> Type -> Type -> Type
pi l n t = pis' [(l, n, t)]

-- | Smart constructor for multiple Sigma applications.
sigmas' :: [(Loc,Name,Type)] -> Type -> Type
sigmas' = q Sigma

-- | Smart constructor for multiple Sigma applications.
sigmas :: [(Loc,Name)] -> Type -> Type -> Type
sigmas ns t = sigmas' (map (\ (l, n) -> (l, n, t)) ns)

-- | Smart constructor for Sigma.
sigma :: Loc -> Name -> Type -> Type -> Type
sigma l n t = sigmas' [(l, n, t)]

-- | Smart constructor for function space.
(->-) :: Type -> Type -> Type
(->-) t = pi (getLoc t) "" t

-- | Smart constructor for product.
(-*-) :: Type -> Type -> Type
(-*-) t = sigma (getLoc t) "" t

-- * Values (well-scoped WHNFs) and environments

-- ** Identifiers

type Id = Int -- index in (potentially multiple) environments

-- ** Scopes

newtype Scope = Scope [(Name, (Id, Maybe (Clos Type)))]
  deriving (Show, Eq)

{-

TODO:

Perhaps return to the following types for scopes:

data Sc a = Scope [(Name,(Id, a))]

type Scope = Sc ()
type Ctx   = Sc (Clos Type)

We then need a forgetful mapping:

fog :: Ctx -> Scope
fog c = map (\ (x,(i,a)) -> (x,(i,()))) c

Alternatively, we can try to make Scopes
existential, essentially:

type Scope = exists a. Sc a

-}

emptyScope :: Scope
emptyScope = Scope []

extendScope :: Scope -> Name -> (Id, Maybe (Clos Type)) -> Scope
extendScope (Scope s) x (i, a) = Scope $ (x, (i, a)) : s

lookupScope :: Scope -> Name -> Maybe Id
lookupScope (Scope s) x = do idCon <- lookup x s
                             return $! fst idCon

lookupCon   :: Scope -> Name -> Maybe (Clos Type)
lookupCon   (Scope s) x = do idCon <- lookup x s
                             return $! fromJust $! snd idCon

-- ** Closures

type Clos a = (a, Scope)

instance GetLoc a => GetLoc (Clos a) where
  getLoc (x, _) = getLoc x

--type C a = (Con,a)

--c2clos :: C a -> Clos a
--c2clos (g,a) = (a,fog g)

-- fix order, name of C...

class Closure a where
  getScope :: a -> Scope
  putScope :: a -> Scope -> a

instance Closure (Clos a) where
  getScope (_, s)   = s
  putScope (a, _) s = (a, s)

instance Closure a => Closure (Bind a) where
  getScope (_, a)   = getScope a
  putScope (x, a) s = (x, putScope a s)

instance Closure Boxed where
  getScope (Boxed c) = getScope c
  putScope (Boxed c) = Boxed . putScope c

{- could be used to refactor the code using bindings. -}

ty :: Clos Type
ty = (Type Unknown, emptyScope)

lty :: Clos Type
lty = (Lift Unknown (Type Unknown), emptyScope)

tforce :: Clos Type -> Clos Type
tforce (a,s) = (Force Unknown a, s)

tlift :: Clos Type -> Clos Type
tlift (a,s) = (Lift Unknown a, s)

label :: Label -> Clos Term
label s = (Label Unknown s, emptyScope)

-- type VType = Val -- unused?

newtype Boxed = Boxed (Clos Term)
  deriving (Show, Eq)

-- ** Values

data Val
  = Ne Ne
  | VType
  | VQ PiSigma (Clos (Type, Bind Type))
  | VLift (Clos Type)
  | VLam (Bind (Clos Term))
  | VPair (Clos (Term, Term))
  | VEnum [Label]
  | VLabel Label
  | VBox Boxed
  | VRec (Clos Type)
  | VFold (Clos Term)
  deriving (Show, Eq)

-- | Neutral terms.
data Ne
  = NVar Id
  | Ne :.. (Clos Term)
  | NSplit Ne (Bind (Bind (Clos Term)))
  | NCase Ne (Clos [(Label, Term)])
  | NForce Ne
  | NUnfold Ne (Bind (Clos Term))
  deriving (Show, Eq)

-- ** Environments

data EnvEntry
  = Id Id
  | Closure (Clos Term)
  deriving Show

data PrtInfo
  =  PrtInfo { name   :: Name
             , expand :: Bool }

class Env e where
  emptyE :: e
  extE   :: e -> PrtInfo -> (Id,e)
  setE   :: e -> Id -> EnvEntry -> e
  getE   :: e -> Id -> EnvEntry
  prtE   :: e -> Id -> PrtInfo

set :: [a] -> Int -> a -> [a]
set []       _ _ = error "list is empty"
set (_ : as) 0 b = b : as
set (a : as) i b = a : set as (i - 1) b

type EnvEntries = [(EnvEntry, PrtInfo)]

instance Env EnvEntries where
  emptyE     = []
  extE e x   = case length e of i    -> (i, e ++ [(Id i, x)])
  setE e i v = case e !! i of (_, x) -> set e i (v, x)
  getE e i   = case e !! i of (v, _) -> v
  prtE e i   = case e !! i of (_, x) -> x
