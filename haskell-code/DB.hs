{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE InstanceSigs, OverloadedStrings #-}

{- | database operations

-- how to represent this if only functions are allowed

put "Mike" 100
x = get "Mike"
put "Mike" (x + 1)
y = get "Mike"
return (show (x+y))
-}
module DB where

import Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import Database.SQLite.Simple

-- Programm als Objekt

type Value = Int
type Key   = String

data DB a = Get Key       (Value -> DB a)
          | Put Key Value (()    -> DB a)
          | Return a

-- | on the way to the Monad
--
example :: DB String
example = Put "Mike" 100 (\() ->
          Get "Mike" (\x ->
          Put "Mike" (x+1) (\() ->
          Get "Mike" (\y ->
          Return (show (x+y))
                        ))))

-- | DB-Programm ablaufen lassen
--
-- >>> runDB example Map.empty
-- ("201",fromList [("Mike",101)])
runDB :: DB a -> Map Key Value -> (a, Map Key Value)
runDB (Get key cont) m =
  let value = m ! key
   in runDB (cont value) m

runDB (Put key value cont) m =
  let m' = Map.insert key value m
   in runDB (cont ()) m'

runDB (Return result) m = (result, m)

get :: Key -> DB Value
get key = Get key Return

put :: Key -> Value -> DB ()
put key value = Put key value Return

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key cont) next =
  Get key       (\value -> splice (cont value) next)
splice (Put key value cont) next =
  Put key value (\_ -> splice (cont ()) next)
splice (Return result) next = next result

example' = splice (put "Mike" 100) (\() ->
           splice (get "Mike") (\x ->
           splice (put "Mike" (x+1)) (\_ ->
           splice (get "Mike") (\y ->
           Return (show (x + y))))))

example'' :: DB String
example'' = do
  put "Mike" 100
  x <- get "Mike"
  put "Mike" (x+1)
  y <- get "Mike"
  return (show (x+y))

instance Functor DB where
  fmap :: (a -> b) -> DB a -> DB b
  fmap f (Get key cont) = Get key (fmap f . cont)
  fmap f (Put key value cont) = Put key value (fmap f . cont)
  fmap f (Return a) = Return (f a)

instance Applicative DB where
  pure = Return

instance Monad DB where
  (>>=) = splice
  return = pure

-- Beispiel "DTO"
-- data TestField = TestField Int String

data TestRow = TestRow Key Value
  deriving Show

instance FromRow TestRow where
  fromRow = TestRow <$> field <*> field

instance ToRow TestRow where
  toRow (TestRow key value) = toRow (key, value)

runDBSQLite :: DB a -> Connection -> IO a
runDBSQLite (Get key cont) connection = do
  [TestRow _ value] <- queryNamed connection "SELECT key, value FROM entries WHERE key = :key" [":key" := key]
  runDBSQLite (cont value) connection

runDBSQLite (Put key value cont) connection = do
  execute connection "REPLACE INTO entries (key, value) VALUES (?, ?)" (TestRow key value)
  runDBSQLite (cont ()) connection
runDBSQLite (Return result) _connection = pure result

-- | run the same program using a sqllite db
--
-- >>> execDB example
-- "201"

execDB :: DB a -> IO a
execDB db = do
  connection <- open "test.db"
  execute_ connection "CREATE TABLE IF NOT EXISTS entries (key TEXT PRIMARY KEY, value INTEGER)"
  result <- runDBSQLite db connection
  close connection
  pure result
