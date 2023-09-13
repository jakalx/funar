{-# LANGUAGE InstanceSigs #-}

module Intro where

-- Groß/Kleinschreibung relevant

x :: Integer
x = 12

y :: Integer
y = x * 3

f :: Integer -> Integer
-- >>> f 5
-- 10
-- f = \ x -> x * 2
-- Abkürzung:
f x = x * 2

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet =
    Dog 
  | Cat
  | Snake
  deriving Show

-- >>> Dog
-- Dog

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool

-- >>> isCute Dog
-- True

-- >>> isCute Snake
-- False

isCute Dog = True 
isCute Cat = True
isCute Snake = False

-- Ein Gürteltier hat folgende Eigenschaften:
-- -- lebendig oder tot
-- -- Gewicht

data Liveness = Alive | Dead
  deriving Show

-- Typsynonym
type Weight = Integer

-- Record-Typ
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- >>> dilloLiveness dillo1
-- Alive

-- >>> dilloWeight dillo2
-- 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- runOverDillo dillo =
--   MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
    MkDillo Dead w