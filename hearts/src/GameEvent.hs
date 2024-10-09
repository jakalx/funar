{-# OPTIONS_GHC -Wno-missing-methods #-}

module GameEvent where

import Cards

import Control.Monad ((>=>))
import Data.Map (Map)
import qualified Data.Map as Map

-- Event: Datenrepräsentation von einem Ereignis

-- - ist passiert, in der Vergangenheit
-- - fachlich
-- - vollständig: gesamte Geschichte der Domäne
-- - Redundanz OK
-- - *nicht* Zustand mit ablegen
{-
data GameEvent =
    CardsDealt (Map Player Hand)
  | GameStarted Player Player Player Player
  | CardLaidAtTrickStart Player Card
  | CardLaidIntoTrick Player Card
  | CardsTaken Player [Card]
  | GameWon Player
  deriving Show

-- Command: Repräsentation eines Wunschs, daß etwas passieren soll - in der Zukunft
-- != Event
data GameCommand =
    DealCards Player [Card]
  | StartGame
  | PlayCard Player Card
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

{-
playCard :: Player -> Card -> ... -> ...

-->

data PlayCardCommand = MkPlayCardCommand Player Card

playCard :: PlayCardCommand -> ... -> ...

playCard p c ... -> playCard (MkPlayerCommand p c) ...

-}

-- als nächstes: Ablauf des Spiels / Anordnung der Events
-- sequentielle Abläufe: Monade

-- Wie werden aus Commands Events?

data Game a
  = RecordEvent GameEvent (() -> Game a)
  | GetCommand (GameCommand -> Game a)
  | IsPlayCardAllowed Player Card (Bool -> Game a)
  | HasRoundEnded ((Player, Maybe Trick) -> Game a)
  | HasGameEnded (Maybe Player -> Game a)
  | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isPlayCardAllowedM :: Player -> Card -> Game Bool
isPlayCardAllowedM player card = IsPlayCardAllowed player card Return

roundEnded :: Game (Player, Maybe Trick)
roundEnded = HasRoundEnded Return

gameEnded :: Game (Maybe Player)
gameEnded = HasGameEnded Return

instance Functor Game

instance Applicative Game where
  pure = Return

instance Monad Game where
  RecordEvent event callback >>= next =
    RecordEvent event (callback >=> next) -- (\x -> callback x >>= next)
  GetCommand callback >>= next = GetCommand (callback >=> next)
  IsPlayCardAllowed player card callback >>= next =
    IsPlayCardAllowed player card (callback >=> next)
  HasRoundEnded callback >>= next = HasRoundEnded (callback >=> next)
  HasGameEnded callback >>= next = HasGameEnded (callback >=> next)
  (>>=) (Return result) next = next result
  return = pure

-- data Maybe a = Nothing | Just a

-- liefert Gewinner:in
tableProcessCommandM :: GameCommand -> Game (Maybe Player) -- sagt uns, ob das Spiel vorbei ist und wer gewonnen hat
tableProcessCommandM (DealHands hands) =
  let list = Map.toList hands
      events = map (uncurry HandDealt) list
      games = map recordEventM events
   in do
        sequence_ games
        return Nothing -- Spiel geht weiter
tableProcessCommandM (PlayCard player card) =
  -- darf der das?
  do
    allowed <- isPlayCardAllowedM player card
    if allowed
      then do
        recordEventM (LegalCardPlayed player card)

        (nextPlayer, maybeTrick) <- roundEnded

        case maybeTrick of
          Nothing -> do
            recordEventM (PlayerTurnChanged nextPlayer)
            return Nothing
          Just trick -> do
            recordEventM (TrickTaken nextPlayer trick)

            maybeWinningPlayer <- gameEnded

            case maybeWinningPlayer of
              Nothing -> do
                recordEventM (PlayerTurnChanged nextPlayer)
                return Nothing
              Just winningPlayer -> do
                recordEventM (GameEnded winningPlayer)
                return $ Just winningPlayer
      else do
        recordEventM (IllegalCardAttempted player card)
        return Nothing

-- gesamtes Spiel spielen
tableLoopM :: GameCommand -> Game Player
tableLoopM command = do
  maybeWinner <- tableProcessCommandM command
  case maybeWinner of
    Nothing -> GetCommand tableLoopM
    Just player -> return player
