{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Intro where

-- Typ
x :: Integer
x = 5

-- data: neuer Typ
-- Literale: groß geschrieben, Variablen: klein
-- | : "oder"

data Pet = Hund | Katze | Schlange
  deriving Show

pet1 :: Pet
pet1 = Hund

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Hund = True
isCute Katze = True
isCute Schlange = False

isCute' :: Pet -> Bool
isCute' pet =
  case pet of 
    Hund -> True
    Katze -> True
    Schlange -> False

-- Zustand eines Gürteltiers zu einem bestimmten Zeitpunkt
data Liveness = Dead | Alive
  deriving Show

{-
data Dillo = Dillo { alive :: Liveness, weight :: Integer}
 deriving Show

-- Gürteltier, lebendig, 10kg
dillo1 :: Dillo
dillo1 = Dillo { alive = Alive, weight = 10}
-- Gürteltier, tot, 12kg
dillo2 :: Dillo
dillo2 = Dillo Dead 12

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
{-
runOverDillo dillo =
--  Dillo { alive = Dead, weight = weight dillo}
   Dillo Dead (weight dillo)
-}
-- runOverDillo (Dillo _ weight) = Dillo Dead weight
runOverDillo (Dillo { weight = w}) =
 Dillo { alive = Dead, weight = w }

data Parrot = Parrot String Integer
  deriving Show

parrot1 = Parrot "Hallo!" 2
-}

-- Algebraischer Datentyp:
-- gemischte Daten aus zusammengesetzte Daten
data Animal = 
    Dillo { alive :: Liveness, weight :: Integer}
  | Parrot String Integer
  deriving Show

dillo1 :: Animal
dillo1 = Dillo { alive = Alive, weight = 10}
dillo2 :: Animal
dillo2 = Dillo Dead 12
parrot1 :: Animal
parrot1 = Parrot "Hallo!" 2
parrot2 :: Animal
parrot2 = Parrot "Goodbye!" 1

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (Dillo _ weight) = Dillo Dead weight
runOverAnimal (Parrot _ weight) = Parrot "" weight

-- Es gibt nur 1stellige Funktionen

-- Tier füttern
feedAnimal :: Integer -> (Animal -> Animal)
{-
feedAnimal amount (Dillo alive weight) = Dillo alive (weight + amount)
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)
-}
-- \ : "Lambda"
feedAnimal = \ amount -> \ animal ->
  case animal of
    Dillo alive weight -> Dillo alive (weight + amount)
    Parrot sentence weight -> Parrot sentence (weight + amount)

-- Tupel

feedAnimal' :: (Integer, Animal) -> Animal
feedAnimal' (amount, Dillo alive weight) = Dillo alive (weight + amount)
feedAnimal' (amount, Parrot sentence weight) = Parrot sentence (weight + amount)

data ListOfIntegers =
    Empty
  | Cons Integer ListOfIntegers

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

-- (: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))
listMap :: (a -> b) -> [a] ->   [b]
listMap    f           []     = []
listMap    f           (x:xs) = (f x) : (listMap f xs)

