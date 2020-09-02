{-# LANGUAGE LambdaCase #-}
module Cube.Facelet.Internal where

import Data.Array (Array, array, listArray, (!))
import Data.Ix (Ix, range)

import Cube

data Facelet =
    ULB | UB | UBR | UR | URF | UF | UFL | UL
  | FLU | FU | FUR | FR | FRD | FD | FDL | FL
  | RFU | RU | RUB | RB | RBD | RD | RDF | RF
  | BRU | BU | BUL | BL | BLD | BD | BDR | BR
  | LBU | LU | LUF | LF | LFD | LD | LDB | LB
  | DLF | DF | DFR | DR | DRB | DB | DBL | DL
  deriving (Eq, Ix, Ord, Show)

newtype FaceletCube = FaceletCube
  { getFaceletCube :: Array Facelet Facelet }
  deriving (Eq, Show)

instance Semigroup FaceletCube where
  (FaceletCube xs) <> (FaceletCube ys) = FaceletCube $
    listArray (ULB, DL) [ys ! (xs ! x) | x <- range (ULB, DL)]

instance Monoid FaceletCube where
  mempty = FaceletCube $ listArray (ULB, DL) $ range (ULB, DL)

instance Group FaceletCube where
  invert (FaceletCube xs) = FaceletCube .
    array (ULB, DL) $ [(xs ! x, x) | x <- range (ULB, DL)]

fromList :: [Facelet] -> Maybe FaceletCube
fromList fs = _faceletCubeFromList

instance Cube FaceletCube where
  fromMove = \case
    U  -> u
    U2 -> u2
    U' -> u'
    F  -> f
    F2 -> f2
    F' -> f'
    R  -> r
    R2 -> r2
    R' -> r'
    B  -> b
    B2 -> b2
    B' -> b'
    L  -> l
    L2 -> l2
    L' -> l'
    D  -> d
    D2 -> d2
    D' -> d'

u :: FaceletCube
u = FaceletCube . listArray (ULB, DL) $
  [ UBR, UR, URF, UF, UFL, UL, ULB, UB
  , LBU, LU, LUF, FR, FRD, FD, FDL, FL
  , FLU, FU, FUR, RB, RBD, RD, RDF, RF
  , RFU, RU, RUB, BL, BLD, BD, BDR, BR
  , BRU, BU, BUL, LF, LFD, LD, LDB, LB
  , DLF, DF, DFR, DR, DRB, DB, DBL, DL
  ]

u2 :: FaceletCube
u2 = FaceletCube . listArray (ULB, DL) $
  [ URF, UF, UFL, UL, ULB, UB, UBR, UR
  , BRU, BU, BUL, FR, FRD, FD, FDL, FL
  , LBU, LU, LUF, RB, RBD, RD, RDF, RF
  , FLU, FU, FUR, BL, BLD, BD, BDR, BR
  , RFU, RU, RUB, LF, LFD, LD, LDB, LB
  , DLF, DF, DFR, DR, DRB, DB, DBL, DL
  ]

u' :: FaceletCube
u' = FaceletCube . listArray (ULB, DL) $
  [ UFL, UL, ULB, UB, UBR, UR, URF, UF
  , RFU, RU, RUB, FR, FRD, FD, FDL, FL
  , BRU, BU, BUL, RB, RBD, RD, RDF, RF
  , LBU, LU, LUF, BL, BLD, BD, BDR, BR
  , FLU, FU, FUR, LF, LFD, LD, LDB, LB
  , DLF, DF, DFR, DR, DRB, DB, DBL, DL
  ]

f :: FaceletCube
f = FaceletCube . listArray (ULB, DL) $
  [ ULB, UB, UBR, UR, RDF, RF, RFU, UL
  , FUR, FR, FRD, FD, FDL, FL, FLU, FU
  , DFR, RU, RUB, RB, RBD, RD, DLF, DF
  , BRU, BU, BUL, BL, BLD, BD, BDR, BR
  , LBU, LU, URF, UF, UFL, LD, LDB, LB
  , LUF, LF, LFD, DR, DRB, DB, DBL, DL
  ]

f2 :: FaceletCube
f2 = FaceletCube . listArray (ULB, DL) $
  [ ULB, UB, UBR, UR, DLF, DF, DFR, UL
  , FRD, FD, FDL, FL, FLU, FU, FUR, FR
  , LFD, RU, RUB, RB, RBD, RD, LUF, LF
  , BRU, BU, BUL, BL, BLD, BD, BDR, BR
  , LBU, LU, RDF, RF, RFU, LD, LDB, LB
  , URF, UF, UFL, DR, DRB, DB, DBL, DL
  ]
  
f' :: FaceletCube
f' = FaceletCube . listArray (ULB, DL) $
  [ ULB, UB, UBR, UR, LUF, LF, LFD, UL
  , FDL, FL, FLU, FU, FUR, FR, FRD, FD
  , UFL, RU, RUB, RB, RBD, RD, URF, UF
  , BRU, BU, BUL, BL, BLD, BD, BDR, BR
  , LBU, LU, DLF, DF, DFR, LD, LDB, LB
  , RDF, RF, RFU, DR, DRB, DB, DBL, DL
  ]

r :: FaceletCube
r = FaceletCube . listArray (ULB, DL) $
  [ ULB, UB, BDR, BR, BRU, UF, UFL, UL
  , FLU, FU, UBR, UR, URF, FD, FDL, FL
  , RUB, RB, RBD, RD, RDF, RF, RFU, RU
  , DRB, BU, BUL, BL, BLD, BD, DFR, DR
  , LBU, LU, LUF, LF, LFD, LD, LDB, LB
  , DLF, DF, FUR, FR, FRD, DB, DBL, DL
  ]

r2 :: FaceletCube
r2 = FaceletCube . listArray (ULB, DL) $
  [ ULB, UB, DFR, DR, DRB, UF, UFL, UL
  , FLU, FU, BDR, BR, BRU, FD, FDL, FL
  , RBD, RD, RDF, RF, RFU, RU, RUB, RB
  , FRD, BU, BUL, BL, BLD, BD, FUR, FR
  , LBU, LU, LUF, LF, LFD, LD, LDB, LB
  , DLF, DF, UBR, UR, URF, DB, DBL, DL
  ]

r' :: FaceletCube
r' = FaceletCube . listArray (ULB, DL) $
  [ ULB, UB, FUR, FR, FRD, UF, UFL, UL
  , FLU, FU, DFR, DR, DRB, FD, FDL, FL
  , RDF, RF, RFU, RU, RUB, RB, RBD, RD
  , URF, BU, BUL, BL, BLD, BD, UBR, UR
  , LBU, LU, LUF, LF, LFD, LD, LDB, LB
  , DLF, DF, BDR, BR, BRU, DB, DBL, DL
  ]

b :: FaceletCube
b = FaceletCube . listArray (ULB, DL) $
  [ LDB, LB, LBU, UR, URF, UF, UFL, UL
  , FLU, FU, FUR, FR, FRD, FD, FDL, FL
  , RFU, RU, ULB, UB, UBR, RD, RDF, RF
  , BUL, BL, BLD, BD, BDR, BR, BRU, BU
  , DBL, LU, LUF, LF, LFD, LD, DRB, DB
  , DLF, DF, DFR, DR, RUB, RB, RBD, DL
  ]

b2 :: FaceletCube
b2 = FaceletCube . listArray (ULB, DL) $
  [ DRB, DB, DBL, UR, URF, UF, UFL, UL
  , FLU, FU, FUR, FR, FRD, FD, FDL, FL
  , RFU, RU, LDB, LB, LBU, RD, RDF, RF
  , BLD, BD, BDR, BR, BRU, BU, BUL, BL
  , RBD, LU, LUF, LF, LFD, LD, RUB, RB
  , DLF, DF, DFR, DR, ULB, UB, UBR, DL
  ]

b' :: FaceletCube
b' = FaceletCube . listArray (ULB, DL) $
  [ RUB, RB, RBD, UR, URF, UF, UFL, UL
  , FLU, FU, FUR, FR, FRD, FD, FDL, FL
  , RFU, RU, DRB, DB, DBL, RD, RDF, RF
  , BDR, BR, BRU, BU, BUL, BL, BLD, BD
  , UBR, LU, LUF, LF, LFD, LD, ULB, UB
  , DLF, DF, DFR, DR, LDB, LB, LBU, DL
  ]

l :: FaceletCube
l = FaceletCube . listArray (ULB, DL) $
  [ FLU, UB, UBR, UR, URF, UF, FDL, FL
  , DLF, FU, FUR, FR, FRD, FD, DBL, DL
  , RFU, RU, RUB, RB, RBD, RD, RDF, RF
  , BRU, BU, UFL, UL, ULB, BD, BDR, BR
  , LUF, LF, LFD, LD, LDB, LB, LBU, LU
  , BLD, DF, DFR, DR, DRB, DB, BUL, BL
  ]

l2 :: FaceletCube
l2 = FaceletCube . listArray (ULB, DL) $
  [ DLF, UB, UBR, UR, URF, UF, DBL, DL
  , BLD, FU, FUR, FR, FRD, FD, BUL, BL
  , RFU, RU, RUB, RB, RBD, RD, RDF, RF
  , BRU, BU, FDL, FL, FLU, BD, BDR, BR
  , LFD, LD, LDB, LB, LBU, LU, LUF, LF
  , ULB, DF, DFR, DR, DRB, DB, UFL, UL
  ]

l' :: FaceletCube
l' = FaceletCube . listArray (ULB, DL) $
  [ BLD, UB, UBR, UR, URF, UF, BUL, BL
  , ULB, FU, FUR, FR, FRD, FD, UFL, UL
  , RFU, RU, RUB, RB, RBD, RD, RDF, RF
  , BRU, BU, DBL, DL, DLF, BD, BDR, BR
  , LDB, LB, LBU, LU, LUF, LF, LFD, LD
  , FLU, DF, DFR, DR, DRB, DB, FDL, FL
  ]

d :: FaceletCube
d = FaceletCube . listArray (ULB, DL) $
  [ ULB, UB, UBR, UR, URF, UF, UFL, UL
  , FLU, FU, FUR, FR, RBD, RD, RDF, FL
  , RFU, RU, RUB, RB, BLD, BD, BDR, RF
  , BRU, BU, BUL, BL, LFD, LD, LDB, BR
  , LBU, LU, LUF, LF, FRD, FD, FDL, LB
  , DFR, DR, DRB, DB, DBL, DL, DLF, DF
  ]

d2 :: FaceletCube
d2 = FaceletCube . listArray (ULB, DL) $
  [ ULB, UB, UBR, UR, URF, UF, UFL, UL
  , FLU, FU, FUR, FR, BLD, BD, BDR, FL
  , RFU, RU, RUB, RB, LFD, LD, LDB, RF
  , BRU, BU, BUL, BL, FRD, FD, FDL, BR
  , LBU, LU, LUF, LF, RBD, RD, RDF, LB
  , DRB, DB, DBL, DL, DLF, DF, DFR, DR
  ]

d' :: FaceletCube
d' = FaceletCube . listArray (ULB, DL) $
  [ ULB, UB, UBR, UR, URF, UF, UFL, UL
  , FLU, FU, FUR, FR, LFD, LD, LDB, FL
  , RFU, RU, RUB, RB, FRD, FD, FDL, RF
  , BRU, BU, BUL, BL, RBD, RD, RDF, BR
  , LBU, LU, LUF, LF, BLD, BD, BDR, LB
  , DBL, DL, DLF, DF, DFR, DR, DRB, DB
  ]
