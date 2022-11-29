{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

-- Zeilenkommentar

x :: Integer
-- >>> x+15 
-- 57
x = 42

y :: Integer
y = x+3

f :: Integer -> Integer
-- >>> f 5
-- 6
f n = n + 1

f' :: Integer -> Integer
-- >>> f' 5
-- 6
f' = \ n -> n + 1

-- Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange

-- data: neuer Datentyp
data Pet
  = Dog -- Konstruktor
  | Cat
  | Snake
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- >>> isCute Cat
-- True
-- >>> isCute Snake
-- False

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht
data Liveness = Alive | Dead
  deriving Show

-- Typalias
type Weight = Integer
{-
data Dillo =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
    deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--  MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- runOverDillo (MkDillo { dilloWeight = w}) =
--  MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- "functional update"
-- Kopie von dillo, nur ist dilloLiveness = Dead
runOverDillo dillo = dillo { dilloLiveness = Dead }
-}

-- algebraischer Datentyp:
-- gemischte Daten, alle Fälle zusammengesetzte Daten
data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 } 

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hello!" 1
parrot2 :: Animal
parrot2 = MkParrot "Goodbye!" 2

-- Laufzeitfehler:
-- >>> dilloLiveness parrot1
-- No match in record selector dilloLiveness

parrotSentence :: Animal -> String
parrotSentence (MkDillo {}) = error "kein Papagei"
parrotSentence (MkParrot sentence _) = sentence

-- Tiere überfahren
runOverAnimal :: Animal -> Animal
-- >>> runOverAnimal parrot1
-- MkParrot "" 1
-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
-- @: Alias-Pattern
runOverAnimal (dillo@MkDillo {}) = dillo { dilloLiveness = Dead }
runOverAnimal (MkParrot _ weight) = MkParrot "" weight

-- Tier füttern
-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> feedAnimal dillo2 5
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
-- Haskell: nur 1stellige Funktionen

-- >>> (feedAnimal dillo1) 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal (MkDillo liveness weight) amount =
    case liveness of
        Alive -> MkDillo liveness (weight+amount)
        Dead -> MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight+amount)

feedAnimal' :: Weight -> Animal -> Animal
-- feedAnimal' animal amount = feedAnimal amount animal
feedAnimal' = swap feedAnimal

feedAnimal'' :: (Animal, Weight) -> Animal
-- >>> feedAnimal''(dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
feedAnimal''(MkDillo liveness weight, amount) =
    case liveness of
        Alive -> MkDillo liveness (weight+amount)
        Dead -> MkDillo liveness weight
feedAnimal''(MkParrot sentence weight, amount) =
    MkParrot sentence (weight+amount)

swap :: (a -> b -> c) -> (b -> a -> c) -- Typvariablen
-- swap f = \ b -> \ a -> f a b
-- swap f = \ b a -> f a b
swap f b a = f a b

untuplify :: ((a, b) -> c) -> (a -> b -> c)
untuplify f a b = f (a, b) 

tuplify :: (a -> b -> c) -> ((a, b) -> c)
tuplify f (a, b) = f a b 

-- Ein Duschprodukt ist eins der folgenden:
-- - Seife, hat pH-Wert
-- - Shampoo, hat Farbe und Haartyp
-- - Duschgel, besteht zu 50% aus Seife, 50% Shampoo

-- - Datentyp
-- - Funktion, die den Seifenanteil berechnet

-- Erweiterung:
-- - Mixtur aus zwei Duschprodukten, beliebige Anteile

type PHWert = Double
data Haartyp = Oily | Dandruffy | Regular
 deriving Show

{-
data Seife = MkSeife PHWert
data Shampoo = MkShampoo Haartyp
  deriving Show
-}

data Duschprodukt =
     MkSeife PHWert
   | MkShampoo Haartyp
--   | MkDuschgel Duschprodukt Duschprodukt
   | MkMixtur
        { mixturProportion1 :: Proportion,
          mixturProdukt1 :: Duschprodukt,
          mixturProportion2 :: Proportion,
          mixturProdukt2 :: Duschprodukt
        }
type Proportion = Double 

mkDuschgel :: Duschprodukt -> Duschprodukt -> Duschprodukt
mkDuschgel produkt1 produkt2 = MkMixtur 0.5 produkt1 0.5 produkt2

seifenAnteil :: Duschprodukt -> Proportion
seifenAnteil (MkSeife _) = 1
seifenAnteil (MkShampoo _) = 0
seifenAnteil (MkMixtur prop1 produkt1 prop2 produkt2) =
  ((seifenAnteil produkt1 * prop1) +
   (seifenAnteil produkt2 * prop2)) / (prop1 + prop2)

