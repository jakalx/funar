{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup(..), Monoid(..))

-- Signatur:
x :: Integer -- feste Typen: groß, Werte klein
x = 7

y :: Integer
y = 12 * x + 5

-- Eine Zahl verdoppeln
-- (: f (number -> number))
f :: Integer -> Integer
-- f = \ n -> n * 2 -- \ ist Lambda!
-- f 0 = 0
-- f 1 = 2
-- f 2 = 4
f n = n * 2

-- Ein Haustier ist eins der Folgenden:
-- - Katze -ODER-
-- - Hund -ODER-
-- - Schlange
data Pet = Cat | Dog | Snake
    deriving Show  -- denkt: toString()

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- 1 Gleichung pro Fall (Racket: eine cond-Klausel pro Fall)
-- Pattern Matching: matchen auf best. Werte von Pet
-- Funktionen in Haskell sind _TOTAL_:
-- - zu jedem Input _muss_ es einen Output geben
isCute Cat = True
isCute Dog = True
isCute Snake = False

-- g n = 2 * n

-- Ein Gürteltier hat folgende Eigenschaften:
-- - Gewicht
-- - Lebenszustand
data Liveness = Alive | Dead
    deriving (Eq, Show)

type Weight = Integer

-- MkDillo == make-dillo : Daten-Konstruktor
-- dilloLiveness == dillo-alive? : Selektor, Accessor
-- dilloWeight == dillo-weight
-- >>> :t MkDillo
-- MkDillo :: Liveness -> (Weight -> Dillo)
-- Geschw. Klammern sind Spezialsyntax für Records
-- data Dillo = MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
--     deriving Show

dillo1 :: Animal
dillo1 = MkDillo Alive 10

dillo2 :: Animal
dillo2 = MkDillo { dilloLiveness = Dead, dilloWeight = 5 }

-- runOverDillo :: Dillo -> Dillo
-- weiß noch nicht, was ich hinschreiben soll? -> undefined
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 5}

-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo Alive weight) = MkDillo Dead weight
-- runOverDillo (MkDillo Dead weight) = MkDillo Dead weight
-- runOverDillo dillo = dillo

-- functional update syntax
-- runOverDillo dillo = dillo { dilloLiveness = Dead }

-- _ : "don't care"
-- runOverDillo (MkDillo { dilloLiveness = _, dilloWeight = weight }) =
-- runOverDillo (MkDillo { dilloWeight = weight }) =
--     MkDillo Dead weight

-- data Dillo = MkDillo Liveness Weight

type Sentence = String

-- gemischte Daten:
-- Ein Tier ist eins der Folgenden:
-- - Dillo
-- - Papagei
data Animal =
    -- MkDillo Dillo -> Problem mit nicht-totalen Funktionen umgehen
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot { parrotSentence :: Sentence, parrotWeight :: Weight }
--   | MkParrot Sentence Weight
  deriving Show

-- Tiere überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkParrot _ weight) = MkParrot "" weight

type Amount = Integer

-- Tiere füttern
feedAnimal :: Animal -> Amount -> Animal
-- Alias-Pattern
feedAnimal dillo@(MkDillo liveness weight) amount =
    -- Fallunterscheidung
    case liveness of
        Dead -> dillo
        Alive -> MkDillo Alive (weight + amount)
feedAnimal parrot amount = parrot { parrotWeight = parrotWeight parrot + amount }

type PhValue = Integer

data HairType = Curly | Straight

-- Duschprodukt ist eins der Folgenden:
-- - Seife (hat pH-Wert)
-- - Shampoo (hat Haartyp)
-- - Duschgel (_IMMER_ 50% Seife, 50% Shampoo)
data ShowerProduct
    = MkSoap PhValue
    | MkShampoo HairType
    | MkShowerGel -- HairType PhValue
    | Mixture Percentage ShowerProduct ShowerProduct

type Percentage = Integer

-- Seifenanteil berechnen
soapPercentage :: ShowerProduct -> Percentage
soapPercentage (MkSoap _) = 100
soapPercentage (MkShampoo _) = 0
soapPercentage MkShowerGel = 50
soapPercentage (Mixture ratio p1 p2) =
    undefined
    -- (soapPercentage p1) / 100 + soapPercentage p2

-- 1) Datenanalyse + Datendefinition
-- 2) Funktion, die den Seifenanteil eines Duschprodukts berechnet
-- 3) - weitere Ausprägung von Duschprodukten:
--      Mixtur aus zwei Duschprodukten, zu beliebigen
--      Anteilen gemischt

-- Division?
-- Selbstbezug?

-- Eine Liste ist eines der Folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste
data ListOf a = -- ListOf a -> a ist "Typparameter"
    Empty
    | Cons a (ListOf a)
    deriving Show

-- Listen in Haskell: eckige Klammern
list1 :: [Integer]
list1 = [5, 6, 7] -- kommasepariert

-- die leere Liste
emptyList :: [a]
emptyList = []

-- Cons?
myCons = 2 : [] -- Cons 2 Empty
-- myCons = [2]

-- Listenelemente summieren
-- >>> listSum [2,3,17]
-- 22
-- >>> listSum []
-- 0
listSum :: [Integer] -> Integer
listSum [] = 0
listSum (x : xs) = x + listSum xs


-- lazy evaluation

-- vs. strikte Evaluation:
--   Funktionsargumente werden _vor_ dem Aufruf der Funktion ausgewertet
--   dann erst wird zur Funktion gesprungen

-- in Haskell: Argumente werden erst dann ausgewertet, wenn sie "benötigt" werden


-- Sieb des Eratosthenes (Primzahlberechnung)

natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n+1)

-- Vielfache einer Zahl streichen
strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n xs =
    filter (\ n' -> n' `mod` n /= 0) xs -- /= ist "ungleich"

-- Sieb der Eratosthenes
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x : xs) = x : (sieve (strikeMultiples x xs))

allPrimes :: [Integer]
allPrimes = let ps = sieve [2..] in ps
    -- let n = 5
    --     m = 2 * n
    -- in m + 1
    -- where
    --     sieve :: ....

-- algebraische Datentypen
-- Ein optionaler Wert ist entweder
-- - vorhanden ("der Wert")
-- - nicht vorhanden ("kein Wert")
data Optional a =
    Result a
    | None
    deriving Show

-- eingebaut!
data Maybe a = Just a | Nothing

-- Index eines Elements in einer Liste ermitteln
-- >>> listIndex "a" ["b", "c", "f"]
-- None
-- >>> listIndex "a" ["b", "c", "a", "bla"]
-- Result 2
listIndex :: Eq a => a -> [a] -> Optional Integer
listIndex elem [] = None
listIndex elem (x : xs) =
    if x == elem
    then Result 0
    else
        case listIndex elem xs of
            None -> None
            Result index -> Result (index + 1)

-- TODO/FIXME: geht das auch eleganter?

-- Typklassen

{-
Eigenschaften eines Typs -> definiert durch Methoden

      v   Typklass
class Eq a where    -- denk: Interface
    (==) :: a -> a -> Bool

-}

-- brauchen Eq für Pet
instance Eq Pet where
    (==) :: Pet -> Pet -> Bool
    (==) Cat Cat = True
    (==) Dog Dog = True
    (==) Snake Snake = True
    (==) _ _ = False

{-

Sinnvoll: Typklassen für _universelle_ Abstraktionen/Konzepte

- Show: "ausprinten"
- Eq: Gleichheit
- Ord: (totale) Ordnung
- Num: "Zahlen mit Operationen"

Algebra:
- Typ T
- Operationen (Methoden) mit Signaturen
- Gesetze/Gleichungen

Leider: Gesetze können (noch) nicht auf dem Typlevel ausgedrückt werden

- Typ T
- Operation:    op :: T -> T -> T ("binäre Operation")
  Assoziativgesetz:    op a (op b c) == op (op a b) c
                       a `op` (b `op` c) == (a `op` b) `op` c

                       Bsp.: a + (b + c) == (a + b) + c
-> Halbgruppe

-}

-- Definition einer Halbgruppe
class Semigroup t where
    -- Es soll das Assoziativgesetz gelten
    op :: t -> t -> t



