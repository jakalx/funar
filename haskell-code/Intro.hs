{-# LANGUAGE RecordWildCards, InstanceSigs, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module Intro where

import Prelude hiding (Monoid, Semigroup, (<>))

import Debug.Trace

x :: Integer
x = 43

{- | @double x@ computes x*2

>>> double 42
84
-}
double :: Integer -> Integer
double = (* 2)

data Pet
    = Dog
    | Cat
    | Snake
    deriving (Show, Eq, Ord)

{- | @isCute Pet@ calculates whether a pet is cute

>>> isCute Cat
True
>>> isCute Dog
True
>>> isCute Snake
False
-}
isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltiere haben folgende Eigenschaften
data Liveness
    = Dead
    | Alive
    deriving (Show, Eq)

type Weight = Integer

{-
data Dillo = MkDillo
    { dilloLiveness :: Liveness
    , dilloWeight :: Weight
    }
    deriving (Show)

{- | runOverDillo kills a dillo

>>> runOverDillo (MkDillo Alive 10)
MkDillo {dilloLiveness = Dead, dilloWeight = Weight 10}
-}
runOverDillo :: Dillo -> Dillo
runOverDillo dillo = dillo{dilloLiveness = Dead}
runOverDillo :: Dillo -> Dillo
runOverDillo dillo = dillo{dilloLiveness = Dead}
-}

data Animal
    = MkDillo
        { dilloLiveness :: Liveness
        , dilloWeight :: Weight
        }
    | MkParrot String Weight
    deriving (Show)

{- | runOverAnimal kills highway animals

>>> runOverAnimal (MkDillo Alive 10)
MkDillo {dilloLiveness = Dead, dilloWeight = 10}
>>> runOverAnimal (MkDillo Dead 10)
MkDillo {dilloLiveness = Dead, dilloWeight = 10}

>>> runOverAnimal (MkParrot "Polly wants a cracker!" 10)
MkParrot "" 10
-}
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _ w) = MkDillo Dead w
runOverAnimal (MkParrot _ w) = MkParrot "" w

{- | feedAnimal feeds an animal so that it gains weight

>>> feedAnimal 10 (MkDillo Alive 10)
MkDillo {dilloLiveness = Alive, dilloWeight = 20}

>>> feedAnimal 10 (MkDillo Dead 10)
MkDillo {dilloLiveness = Dead, dilloWeight = 10}

>>> feedAnimal 10 (MkParrot "Polly wants a cracker!" 10)
MkParrot "Polly wants a cracker!" 20
-}
feedAnimal :: Integer -> Animal -> Animal
feedAnimal food dillo@MkDillo{..} = case dilloLiveness of
    Alive -> dillo{dilloWeight = dilloWeight + food}
    Dead -> dillo
feedAnimal food parrot@(MkParrot s w) = MkParrot s (w + food)

-- Eine geometrische Figur "Shape" ist eins der folgenden
--   - Circle
--   - Quadrat
--   - Mix zweier geometrischer Figuren

-- 1. Datatype
-- 2. Funktion, die ermittelt, ob ein Punkt innerhalb einer Shape liegt oder nicht

type Point = (Double, Double)
type Radius = Double
type Length = Double

data Shape = Circle Point Radius -- Circle :: Point -> Radius -> Shape
           | Quad Point Length
           | Overlap Shape Shape
           deriving Show

-- | isWithinShape checks whether a point is within a shape
--
-- >>> isWithinShape (0, 0) (Quad (0, 0) 1)
-- True
-- >>> isWithinShape (0.5, 0.5) (Circle (0, 0) 1)
-- True
-- >>> isWithinShape (2, 2) (Circle (0, 0) 1)
-- False
-- >>> isWithinShape (0.5, 0.5) (Overlap (Circle (0, 0) 1) (Quad (0, 0) 1))
-- True
-- >>> isWithinShape (2, 2) (Overlap (Circle (0, 0) 1) (Quad (0, 0) 3))
-- True
-- >>> isWithinShape (1, 1) (Overlap (Circle (0, 0) 1) (Quad (0, 0) 1))
-- True
isWithinShape :: Point -> Shape -> Bool
isWithinShape (x, y) (Circle (x0, y0) r) =
  let
    dx = x0 - x
    dy = y0 - y
  in dx^2 + dy^2 <= r^2

isWithinShape (x, y) (Quad (x0, y0) l) =
  x <= (x0 + l) && x >= x0 && y <= (y0 + l) && y >= y0
isWithinShape p (Overlap s1 s2) = isWithinShape p s1 || isWithinShape p s2

-- a list is either empty or contains an element

data List a = Nil
            | Cons a (List a)
            deriving (Show)

list1 = Cons 5 (Cons 3 (Cons 4 Nil))

listFold :: b -> (a -> b -> b) -> [a] -> b
listFold b _ [] = b
listFold b f (x:xs) = f x (listFold b f xs)

-- |
--
-- >>> listFold 0 (+) [1 .. 10]
-- 55


-- | swap reimplements flip
--
-- >>> swap (++) "foo" "bar"
-- "barfoo"
swap :: (a -> b -> c) -> (b -> a -> c)
swap f b a = f a b

-- | reimplement uncurry
-- >>> tuplify (+) (4, 5)
-- 9
tuplify :: (a -> b -> c) -> ((a, b) -> c)
tuplify f (a, b) = f a b

-- | reimplement curry
-- >>> untuplify (tuplify (+)) 4 5
-- 9
untuplify :: ((a, b) -> c) -> (a -> b -> c)
untuplify f a b = f (a, b)

-- | listIndex locates an element within a list
-- >>> listIndex 1 []
-- Nothing
-- >>> listIndex 1 [2, 1]
-- Just 1
-- >>> listIndex Cat [Cat, Dog, Snake]
-- Just 0
listIndex :: Eq a => a -> [a] -> Maybe Integer
listIndex _ [] = Nothing
listIndex a (x:xs) = if a == x then Just 0 else (+1) <$> listIndex a xs

--

data Option a = None
              | Some a
              deriving (Show, Eq)

class Semigroup a where
  op :: a -> a -> a

(<>) :: Semigroup a => a -> a -> a
(<>) = op

class Semigroup a => Monoid a where
  neutral :: a

instance Semigroup [a] where
  op :: [a] -> [a] -> [a]
  op = (++)

instance Monoid [a] where
  neutral :: [a]
  neutral = []

-- | implement Semigroup for Option a
--
-- >>> Some "hello " `op` Some "world"
-- Some "hello world"
--
-- >>> Some "hello" `op` neutral == Some "hello"
-- True
--
-- >>> neutral `op` Some "hello" == Some "hello"
-- True
--
-- >>> neutral `op` neutral == (neutral :: Option String)
-- True
--
instance Semigroup a => Semigroup (Option a) where
  op :: Option a -> Option a -> Option a
  op None     x        = x
  op x        None     = x
  op (Some a) (Some b) = Some (op a b)

-- | implement Monoid for Option a
--
-- >>> Some "hello " `op` Some "world"
-- Some "hello world"
--
-- >>> Some "hello" `op` neutral == Some "hello"
-- True
--
-- >>> neutral `op` Some "hello" == Some "hello"
-- True
--
-- >>> neutral `op` neutral == (neutral :: Option String)
-- True
--
instance Semigroup a => Monoid (Option a) where
  neutral :: Option a
  neutral = None

-- | Last returns the last Some
--
-- >>> getLast $ Last (Some "foo") `op` Last (Some "bar")
-- Some "bar"
--
-- >>> getLast $ Last None `op` Last (Some "bar")
-- Some "bar"
newtype Last a = Last { getLast :: Option a }
instance Semigroup (Last a) where
  op :: Last a -> Last a -> Last a
  op x (Last None)       = x
  op _ x@(Last (Some _)) = x

instance Monoid (Last a) where
  neutral :: Last a
  neutral = Last None

-- | First returns the first Some
--
-- >>> getFirst $ First (Some "foo") `op` First (Some "bar")
-- Some "foo"
--
-- >>> getFirst $ First None `op` First (Some "bar")
-- Some "bar"
newtype First a = First { getFirst :: Option a }
instance Semigroup (First a) where
  op :: First a -> First a -> First a
  op x@(First (Some _)) _ = x
  op (First None) x       = x

instance Monoid (First a) where
  neutral :: First a
  neutral = First None

-- | implement Functor for Option
--
-- >>> (+1) <$> Some 1
-- Some 2
-- >>> length <$> Some "Hello"
-- Some 5
-- >>> id <$> Some 1
-- Some 1
instance Functor Option where
  fmap :: (a -> b) -> Option a -> Option b
  fmap f None     = None
  fmap f (Some a) = Some (f a)


-- Validation Applicative

type Errors = [String]

data Validation m a = Valid a
                  | Invalid m
                  deriving Show

instance Functor (Validation m) where
  fmap :: (a -> b) -> (Validation m a -> Validation m b)
  fmap f (Valid a) = Valid (f a)
  fmap _ (Invalid errors) = Invalid errors

instance Semigroup m => Applicative (Validation m) where
  pure :: a -> Validation m a
  pure = Valid

  (<*>) :: Validation m (a -> b) -> Validation m a -> Validation m b
  Valid f <*> Valid a = Valid (f a)
  Valid f <*> Invalid errors = Invalid errors
  Invalid errors <*> Valid a = Invalid errors
  Invalid e1 <*> Invalid e2 = Invalid (e1 <> e2)


-- Aufgabe:
--   - Applicative instanz
--   - kleines Beispiel: Person mit Alter und Name, Alter >18, Name <100 Buchstaben
type PLZ = Int
data Person = Person String Int PLZ
  deriving Show

parseName :: String -> Validation Errors String
parseName name =
  if length name < 100 then Valid name else Invalid ["name is too long"]

parseAge :: Int -> Validation Errors Int
parseAge age =
  if age >= 18 then Valid age else Invalid ["must be at least 18"]

parsePLZ :: Int -> Validation Errors PLZ
parsePLZ plz =
  if length (show plz) == 5 then Valid plz else Invalid ["PLZ must have 5 digits"]

-- | pares a person
--
-- >>> parsePerson "Alex" 19 29664
-- Valid (Person "Alex" 19 29664)
--
-- >>> parsePerson "Alex" 15 29664
-- Invalid ["must be at least 18"]
--
-- >>> parsePerson "Alex" 15 29
-- Invalid ["must be at least 18","PLZ must have 5 digits"]
--
-- >>> parsePerson (replicate 100 'a') 15 29
-- Invalid ["name is too long","must be at least 18","PLZ must have 5 digits"]
parsePerson :: String -> Int -> PLZ -> Validation Errors Person
parsePerson name age plz =
  -- Person ::   String ->          Int ->          PLZ        -> Person
  Person <$> parseName name <*> parseAge age <*> parsePLZ plz
