-- | implements game events

module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: data representation of events within the system (here the hearts game)
-- events are:
--  - an event already happened, it represents a fact
--  - they describe domain specific things
--  - complete, i.e. they contain all information that is needed (whole history of the domain)
--  - redundancy (data duplication) is perfectly ok
--  - do not carry state!

data GameEvent = CardsDealt (Map Player Hand)
               | GameStarted Player Player Player Player
               | CardLaidAtTrickStart Player Card
               | CardLaidIntoTrick Player Card
               | CardsTaken Player [Card]
               | GameWon Player
               deriving (Show)

-- Command: Representation of a *wish* that something should happen
-- commands are:
--  -  not events!

data GameCommand = DealCards Player [Card]
                 | StartGame
                 | PlayCard Player Card

-- Ablauf des Spiels, Anordnung der Events
--  - sequential flow (using Monads)
