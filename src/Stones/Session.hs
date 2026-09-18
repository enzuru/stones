-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | One game, and what each thing the player does turns it into.
--
-- A window holds several of these, one per tab, each with an opponent
-- of its own. Nothing here knows about tabs or about widgets: a step
-- takes a game and an event and answers with the game as it now
-- stands, and with what to ask the opponent, if anything. The window
-- is what runs the asking and hands the answer back.
module Stones.Session
  ( Session(..)
  , Opponent(..)
  , Note(..)
  , SessionEvent(..)
  , Reply(..)
  , Step(..)
  , starting
  , opened
  , couldNotOpen
  , step
  , ready
  , playable
  , canUndo
  , opponentOf
  , sessionSize
  )
where

import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )

import           Go.Game
import           Go.Types
import           Stones.Engine

-- | Where a game's opponent is up to.
--
-- The four states are what the window has to tell apart, and a game is
-- in exactly one of them. Saying the same thing with a handful of
-- flags would let a game be starting and broken at once, which is not
-- a thing that can happen and which the window would then have to have
-- an opinion about.
data Opponent
  = Starting
    -- ^ A program is being started for this game. Nothing can be
    -- played until it is.
  | Idle Engine
    -- ^ It is there, and nothing has been asked of it.
  | Waiting Engine
    -- ^ It has been asked something and has not answered yet.
  | Gone Text
    -- ^ It failed, and this is what to tell the player. A game whose
    -- opponent is gone takes no more moves.

-- | One game of a window.
data Session = Session
  { sessionGame     :: Game
    -- ^ The position, and whose turn it is.
  , sessionOpponent :: Opponent
  , sessionHuman    :: Color
    -- ^ The colour the player at this window has in this game.
  , sessionNote     :: Maybe Note
    -- ^ Something to say about this game, if there is anything.
  }

-- | What the window has to say about a game beyond whose turn it is.
--
-- Everything else it shows is worked out from the game and from its
-- opponent: that the engine is thinking, that the game is over, that
-- the engine is gone and why. These two are what would otherwise have
-- nowhere to live, because neither is written anywhere on the board.
data Note
  = Refused Illegal
    -- ^ The last click was not a move the rules allow.
  | Result Text
    -- ^ What the opponent made the score, once the game had ended.
  deriving (Eq, Show)

-- | What the player does to a game.
data SessionEvent
  = Clicked Coord
  | Passed
  | ResignPressed
  | UndoPressed
  | Answered Reply
    -- ^ The opponent answered what it was last asked.
  deriving (Show)

-- | What an opponent answered.
data Reply
  = Moved Move
    -- ^ It played this.
  | Ready
    -- ^ It did as it was told and has nothing to say.
  | Scored Text
    -- ^ This is what it makes the score.
  | Failed Text
    -- ^ It could not do as it was told.
  deriving (Show)

-- | Where a game stands after an event, and what to ask its opponent.
--
-- The asking is an action rather than a started thread, so that this
-- module stays a function of its arguments and the window decides when
-- and where it runs.
data Step = Step
  { stepSession :: Session
  , stepAsk     :: Maybe (IO Reply)
  }

-- | A game on a board this wide, waiting for an opponent to be
-- started for it.
starting :: Color -> Int -> Session
starting human n = Session { sessionGame     = newGame n
                           , sessionOpponent = Starting
                           , sessionHuman    = human
                           , sessionNote     = Nothing
                           }

-- | The opponent this game was waiting for has started.
--
-- A player with White has the opponent open the game, so the first
-- thing that happens to this game is a question rather than a click.
opened :: Engine -> Session -> Step
opened engine session
  | gameTurn game == sessionHuman session = stay
    session { sessionOpponent = Idle engine }
  | otherwise = asking
    session
    engine
    (either Failed Moved <$> engineGenMove engine (gameTurn game))
  where game = sessionGame session

-- | No opponent could be started for this game.
couldNotOpen :: Text -> Session -> Session
couldNotOpen why session = session { sessionOpponent = Gone why }

-- | The width of the board this game is played on.
sessionSize :: Session -> Int
sessionSize = boardSize . sessionGame

-- | What to call this game's opponent.
opponentOf :: Session -> Text
opponentOf session = case sessionOpponent session of
  Starting  -> "starting"
  Idle    e -> engineName e
  Waiting e -> engineName e
  Gone    _ -> "no engine"

-- | The opponent to send a move to, if the player can move right now.
--
-- Answering with the engine rather than with a yes or a no is what
-- lets the moves below be written without a branch for the case where
-- the player may move and there is nothing to move against.
ready :: Session -> Maybe Engine
ready session = case sessionOpponent session of
  Idle engine
    | not (finished game), gameTurn game == sessionHuman session -> Just engine
  _ -> Nothing
  where game = sessionGame session

-- | Can the player move in this game? This is 'ready' with the
-- opponent left out, for the window, which only wants to know whether
-- a button is live.
playable :: Session -> Bool
playable = isJust . ready

-- | Is there anything to take back, and is the opponent free to be
-- told about it?
canUndo :: Session -> Bool
canUndo session = case sessionOpponent session of
  Idle _ -> isJust (rewind (sessionHuman session) (sessionGame session))
  _      -> False

-- * Stepping
-------------

-- | What one event does to a game.
step :: Session -> SessionEvent -> Step
step session = \case
  Clicked coord  -> humanMove session (Play coord)
  Passed         -> humanMove session Pass
  ResignPressed  -> resign session
  UndoPressed    -> takeBack session
  Answered reply -> answered session reply

-- | The game stands here, and nothing is asked of anybody.
stay :: Session -> Step
stay session = Step session Nothing

-- | The game stands here, and the opponent has been asked this.
--
-- Anything the window had to say is cleared: a refusal is about the
-- click before this one, and a score is about a game that has ended,
-- which is not a game anybody is being asked to move in.
asking :: Session -> Engine -> IO Reply -> Step
asking session engine job = Step
  session { sessionOpponent = Waiting engine, sessionNote = Nothing }
  (Just job)

-- | The player's own move.
humanMove :: Session -> Move -> Step
humanMove session move = case ready session of
  Nothing     -> stay session
  Just engine -> case playMove human move (sessionGame session) of
    Left reason -> stay session { sessionNote = Just (Refused reason) }
    Right game
      | finished game -> asking session { sessionGame = game }
                                engine
                                (tellThenScore engine human move)
      | otherwise -> asking session { sessionGame = game }
                            engine
                            (tellThenMove engine human move)
  where human = sessionHuman session

-- | Giving the game up, which ends it where it stands.
--
-- There is nothing to tell the opponent: the protocol has no command
-- for a resignation, and there is no next move to ask for.
resign :: Session -> Step
resign session = case ready session of
  Nothing     -> stay session
  Just engine -> case playMove human Resign (sessionGame session) of
    Left  _    -> stay session
    Right game -> stay session { sessionGame     = game
                               , sessionOpponent = Idle engine
                               , sessionNote     = Nothing
                               }
  where human = sessionHuman session

-- | Take back moves until it is the player's turn again, on this
-- board and on the opponent's.
--
-- One move is not enough: taking back only the opponent's answer would
-- leave the player looking at their own move with the opponent about
-- to answer it again. Two is the usual number, and one is what is
-- there to take back when the opponent opened the game.
takeBack :: Session -> Step
takeBack session = case sessionOpponent session of
  Idle engine -> case rewind human (sessionGame session) of
    Nothing            -> stay session
    Just (count, game) -> asking session { sessionGame = game }
                                 engine
                                 (undoThen engine count (gameTurn game) human)
  _ -> stay session
  where human = sessionHuman session

-- | What the opponent answered.
answered :: Session -> Reply -> Step
answered session reply = case sessionOpponent session of
  -- An answer with nobody waiting for it belongs to a game that has
  -- moved on since the question, so there is nothing to do with it.
  Waiting engine -> case reply of
    Moved move -> opponentMove session engine move
    Ready      -> stay (freed engine session)
    Scored out -> stay (freed engine session) { sessionNote = Just (Result out) }
    Failed why -> stay session { sessionOpponent = Gone why }
  _ -> stay session

-- | The opponent has answered and is free again.
freed :: Engine -> Session -> Session
freed engine session = session { sessionOpponent = Idle engine }

-- | The opponent's move, which is played on this board too.
opponentMove :: Session -> Engine -> Move -> Step
opponentMove session engine move = case move of
  Resign -> stay (freed engine session { sessionGame = resigned })
  _      -> case playMove them move (sessionGame session) of
    -- The opponent has answered with something this program's rules do
    -- not allow, which means the two boards have drifted apart. Saying
    -- so is the only honest thing left to do.
    Left reason -> stay session { sessionOpponent = Gone (disagreement reason) }
    Right game
      | finished game -> asking session { sessionGame = game }
                                engine
                                (askScore engine)
      | otherwise     -> stay (freed engine session { sessionGame = game })
 where
  them = opposite (sessionHuman session)
  disagreement reason =
    "The engine and the board disagree: " <> describeIllegal reason
  resigned = case playMove them Resign (sessionGame session) of
    Right game -> game
    Left  _    -> sessionGame session

-- | How far back to go, and where that lands, taking at most two moves
-- off.
rewind :: Color -> Game -> Maybe (Int, Game)
rewind human = go 0
 where
  go taken game
    | taken > 0 && gameTurn game == human = Just (taken, game)
    | taken >= 2 = Just (taken, game)
    | otherwise = case undoMove game of
      Nothing     -> if taken > 0 then Just (taken, game) else Nothing
      Just before -> go (taken + 1) before

-- * What gets asked of an opponent
-----------------------------------

-- | Tell it what was played, then ask it for its own move.
tellThenMove :: Engine -> Color -> Move -> IO Reply
tellThenMove engine human move = engineNotify engine human move >>= \case
  Left  why -> pure (Failed why)
  Right ()  -> either Failed Moved <$> engineGenMove engine (opposite human)

-- | Tell it what was played, then ask it what the score is. This is
-- the second pass of a game that has just ended.
tellThenScore :: Engine -> Color -> Move -> IO Reply
tellThenScore engine human move = engineNotify engine human move >>= \case
  Left  why -> pure (Failed why)
  Right ()  -> askScore engine

-- | Ask it what the score is.
askScore :: Engine -> IO Reply
askScore engine = either Failed Scored <$> engineScore engine

-- | Take it back this many moves, and ask it to move if the board it
-- is left with is one where it has the turn.
undoThen :: Engine -> Int -> Color -> Color -> IO Reply
undoThen engine count turn human = engineUndo engine count >>= \case
  Left why -> pure (Failed why)
  Right ()
    | turn == human -> pure Ready
    | otherwise     -> either Failed Moved <$> engineGenMove engine turn
