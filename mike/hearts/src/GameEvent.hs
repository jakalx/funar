module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

{-
Events:
- Beschreibung eines Ereignisses in der Vergangenheit
- erzählen Geschichte der Domäne - die gesamte Geschichte
- alles, was wir über ein Ereignis weiß
- Redundanz OK
- fachlich
- enthält nicht den Zustand

vs. Commands (optional)
- Beschreibung eines Wunsches, in der Zukunft
-}

{-
data GameEvent =
    CardPlayed Card Player
  | CardsShuffled [Card]
  | GameStarted [Player]
  | GameEnded Player
  | RoundEnded Player Trick
  | CardsDealt (Map Player Hand)
  | PlayerTurnChanged Player

data GameCommand =
    Cheat Player Card
  | PlayCard Player Card
  | TakeTrick Player Trick
  | StartGame [Player]
  | ShuffleCards [Card]
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

data GameCommand
  = DealHands (Map Player Hand)
  | PlayCard Player Card
  deriving (Show, Eq)

-- | Steht in den Events, wer gewonnen hat?
-- >>> let mike = Player "Mike"
-- >>> let peter = Player "Peter"
-- >>> eventsWinner []
-- Nothing
-- >>> eventsWinner [PlayerTurnChanged mike]
-- Nothing
-- >>> eventsWinner [PlayerTurnChanged peter, GameEnded mike]
-- Just (Player {playerName = "Mike"})
eventsWinner :: [GameEvent] -> Maybe Player
eventsWinner [] = Nothing
eventsWinner (first : rest) =
  case first of
    GameEnded winner -> Just winner
    _ -> eventsWinner rest
