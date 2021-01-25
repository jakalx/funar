{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- WICHTIG:
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Diagram where

import Data.Monoid
import Data.Semigroup

-- combine heißt jetzt <>
-- neutral heißt jetzt 

-- Größe mit inbegriffen + Position
data Prim = Square | Circle | Triangle | Smiley
  deriving Show

-- Idee: primitive Bilder übereinanderlegen
-- oberstes Bild kommt zuletzt
-- type Diagram = [Prim]

-- Brent Yorgey: Bilder müssen Monoid bilden
-- Listen bilden einen Monoiden
-- hätte gern eine Monoid-Operation, beim dem das oberste Bild zuerst kommt

-- => Monoid Diagram, aber der falsche Monoid
-- [p1, p2, p3] <> [p4, p5] = [p1, p2, p3, p4, p5]

-- deswegen nächster Schritt:

{-
data Diagram = Diagram [Prim]

instance Semigroup Diagram where
    (Diagram prims1) <> (Diagram prims2) =  Diagram (prims2 ++ prims1)
-}

-- Geht auch einfacher:

newtype Diagram = Diagram (Dual [Prim])
  -- übernimmt Instanzen von Dual
  deriving (Semigroup, Monoid)

-- Strategie:
-- den Typ Diagram komplett mit schon vorhandenen
-- Monoid-Instanzen programmieren

-- nächste Idee:
-- wir berechnen für jedes Bild ein sogenanntes
-- "Envelope": Um wieviel muß ich einen Vektor strecken,
-- damit die Senkrechte dazu gerade das Bild streift?

type R = Double -- reelle Zahlen
type V2 = (R, R)

-- 1. Entwurf:
{-
newtype Envelope = Envelope (V2 -> R)

instance Semigroup Envelope ...
-}
-- envelope :: Diagram -> Envelope

-- envelope (d1 <> d2) = envelope d1 <> envelope d2

-- Da Diagramme einen Monoiden bilden,
-- bilden auch die Envelopes einen Monoiden
-- => Das geht einfach, indem das Maximum des
-- Ergebnisses der Envelope-Funktion gebildet wird.

{-
newtype Max t = Max t

instance Ord t => Semigroup (Max t) where
    combine a b =
        if a > b
        then a
        else b

-}

-- a -> m, wobei a = V2, m = Max R
newtype Envelope = Envelope (V2 -> Max R)
  deriving (Semigroup)

-- blöd: kein Monoid, weil Max kein Monoid ist
-- R plus "minus unendlich" könnte man versuchen,
-- aber häßlich