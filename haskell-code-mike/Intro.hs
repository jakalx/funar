module Intro where

x :: Integer
x = 10

-- eigener Datentyp
data Pet = Dog | Cat | Snake
  deriving Show

-- Faustregel: Großbuchstaben - Konstante, Kleinbuchstaben - Variable

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Typen und Werte haben unterschiedliche Namensräume

-- Ein Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht
data Dillo = Dillo { dilloAlive :: Bool,
                     dilloWeight :: Double }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = Dillo { dilloAlive = True, dilloWeight = 10}

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = Dillo False 8