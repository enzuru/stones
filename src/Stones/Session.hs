-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | One game, and what each thing that happens turns it into.
--
-- A window holds several of these, one per tab, each with an opponent
-- of its own. Nothing here knows about tabs or about widgets: a step
-- takes a game and an event and answers with the game as it now
-- stands, and with what to say to the opponent, if anything. The
-- window is what says it, and what carries back whatever arrives.
module Stones.Session
  ( Session(..)
  , Standing(..)
  , Note(..)
  , Chat(..)
  , SessionEvent(..)
  , Step
  , starting
  , opened
  , couldNotOpen
  , step
  , ready
  , playable
  , canUndo
  , listening
  , opponentOf
  , sessionSize
  )
where

import           Data.Maybe                     ( isJust )
import           Data.Text                      ( Text )

import           GI.Gtk.Declarative.App.Simple  ( Transition(..)
                                                , none
                                                , perform
                                                )
import           Go.Game
import           Go.Types
import           Stones.Opponent

-- | Where a game's opponent is up to.
--
-- Three states, and a game is in exactly one of them. Whose turn it is
-- is not one of them, because the game already knows that, and two
-- places that both claim to know it are two places that can disagree.
data Standing
  = Starting
    -- ^ One is being found for this game. Nothing can be played until
    -- there is.
  | Here Opponent
    -- ^ There is one, and the game is on.
  | Gone Text
    -- ^ It failed, and this is what to tell the player. A game whose
    -- opponent is gone takes no more moves.

-- | Something somebody said about this game.
data Chat = Chat
  { who  :: Text
  , what :: Text
  }
  deriving (Eq, Show)

-- | One game of a window.
data Session = Session
  { game     :: Game
    -- ^ The position, and whose turn it is.
  , standing :: Standing
  , human    :: Color
    -- ^ The colour the player at this window has in this game.
  , note     :: Maybe Note
    -- ^ Something to say about this game, if there is anything.
  , said     :: [Chat]
    -- ^ What has been said about it, oldest first.
  }

-- | What the window has to say about a game beyond whose turn it is.
--
-- Everything else it shows is worked out from the game and from its
-- opponent: that the opponent is thinking, that the game is over, that
-- it is gone and why. These two are what would otherwise have nowhere
-- to live, because neither is written anywhere on the board.
data Note
  = Refused Illegal
    -- ^ The last click was not a move the rules allow.
  | Result Text
    -- ^ What the game came to, once it had ended.
  deriving (Eq, Show)

-- | What happens to a game.
data SessionEvent
  = Clicked Coord
  | Passed
  | ResignPressed
  | UndoPressed
  | Says Text
    -- ^ The player wrote something to say.
  | Arrived Heard
    -- ^ Something came from the other side.
  deriving (Show)

-- | Where a game goes when something happens to it.
--
-- A game is a part of a window, so it answers with a transition of its
-- own, which the window lifts into one of its own with 'bimap'. The
-- 'GI.Gtk.Declarative.App.Simple.Exit' case belongs to the window: a
-- game never ends the program, and nothing here answers with it.
type Step = Transition Session SessionEvent

-- | A game on a board this wide, waiting for an opponent.
starting :: Color -> Int -> Session
starting human n = Session { game     = newGame n
                           , standing = Starting
                           , human    = human
                           , note     = Nothing
                           , said     = []
                           }

-- | The opponent this game was waiting for is here.
--
-- A player with White has the other side open the game, so the first
-- thing that happens is something said rather than something clicked.
opened :: Opponent -> Session -> Step
opened them session
  | session.game.turn == session.human = stay session { standing = Here them }
  | otherwise = telling session { standing = Here them } them [Asked]

-- | No opponent could be found for this game.
couldNotOpen :: Text -> Session -> Session
couldNotOpen why session = session { standing = Gone why }

-- | The width of the board this game is played on.
sessionSize :: Session -> Int
sessionSize session = boardSize session.game

-- | What to call this game's opponent.
opponentOf :: Session -> Text
opponentOf session = case session.standing of
  Starting -> "starting"
  Here it  -> it.name
  Gone _   -> "nobody"

-- | The opponent of a game that is still listening, which is the one
-- the window keeps a subscription open for.
--
-- A game that never started and a game whose opponent is gone have
-- nothing to listen to.
listening :: Session -> Maybe Opponent
listening session = case session.standing of
  Here it -> Just it
  _       -> Nothing

-- | The opponent to say a move to, if the player can move right now.
--
-- Answering with the opponent rather than with a yes or a no is what
-- lets the moves below be written without a branch for the case where
-- the player may move and there is nobody to move against.
ready :: Session -> Maybe Opponent
ready session = case session.standing of
  Here it | not (finished game), game.turn == session.human -> Just it
  _ -> Nothing
  where game = session.game

-- | Can the player move in this game?
playable :: Session -> Bool
playable = isJust . ready

-- | Is there anything to take back, and is the opponent there to be
-- told about it?
canUndo :: Session -> Bool
canUndo session = case session.standing of
  Here _ -> isJust (rewind session.human session.game)
  _      -> False

-- * Stepping
-------------

-- | What one event does to a game.
step :: Session -> SessionEvent -> Step
step session = \case
  Clicked coord -> humanMove session (Play coord)
  Passed        -> humanMove session Pass
  ResignPressed -> resign session
  UndoPressed   -> takeBack session
  Says text     -> says session text
  Arrived heard -> arrived session heard

-- | The game stands here, and nothing is said to anybody.
stay :: Session -> Step
stay session = Transition session none

-- | The game stands here, and these things are being said to the
-- opponent, in this order.
--
-- Anything the window had to say is cleared: a refusal is about the
-- click before this one, and a score is about a game that has ended,
-- which is not a game anybody is being asked to move in.
--
-- The job runs under no name. A named job stops whatever is running
-- under that name, and what is said here is several lines of protocol:
-- one stopped between two of them would leave the answer to the first
-- sitting in the pipe, to be read as the answer to whatever was said
-- next.
telling :: Session -> Opponent -> [Told] -> Step
telling session them things =
  Transition session { note = Nothing } (perform (saying them things))

-- | Say each of these, and stop at the first that will not be said.
--
-- A thing that will not be said is the opponent going, because there
-- is nothing else it can be: the game here has already moved on, and
-- an opponent that did not hear about it is one this program can no
-- longer play against.
saying :: Opponent -> [Told] -> IO (Maybe SessionEvent)
saying them = go
 where
  go []             = pure Nothing
  go (this : after) = them.tell this >>= \case
    Left  why -> pure (Just (Arrived (Lost why)))
    Right ()  -> go after

-- | The player's own move, which goes down here first and is said
-- afterwards.
humanMove :: Session -> Move -> Step
humanMove session move = case ready session of
  Nothing   -> stay session
  Just them -> case playMove session.human move session.game of
    Left  reason -> stay session { note = Just (Refused reason) }
    Right game
      | finished game -> telling session { game = game } them [Moved move, Counting]
      | otherwise     -> telling session { game = game } them [Moved move, Asked]

-- | Giving the game up, which ends it where it stands.
resign :: Session -> Step
resign session = case ready session of
  Nothing   -> stay session
  Just them -> case playMove session.human Resign session.game of
    Left  _    -> stay session
    Right game -> telling session { game = game } them [Resigned]

-- | Something to say to whoever is listening.
--
-- It is not written down here. What was said is what comes back, so a
-- game shows the same words whether this program or the other side put
-- them there, and shows them in the order everybody else sees.
says :: Session -> Text -> Step
says session text = case session.standing of
  Here it -> Transition session (perform (saying it [Said text]))
  _       -> stay session

-- | Take back moves until it is the player's turn again, here and on
-- the other side.
--
-- One move is not enough: taking back only the opponent's answer would
-- leave the player looking at their own move with the opponent about
-- to answer it again. Two is the usual number, and one is what is
-- there to take back when the opponent opened the game.
takeBack :: Session -> Step
takeBack session = case session.standing of
  Here them -> case rewind session.human session.game of
    Nothing            -> stay session
    Just (count, game) -> telling session { game = game } them
      (TookBack count : [ Asked | game.turn /= session.human ])
  _ -> stay session

-- * What arrives
-----------------

-- | Something came from the other side.
arrived :: Session -> Heard -> Step
arrived session = \case
  Began opening     -> began session opening
  Played color move -> theirMove session color move
  Spoke person text ->
    stay session { said = session.said <> [Chat person text] }
  Undid count       -> tookBack session count
  Counted out       -> stay session { note = Just (Result out) }
  Lost why          -> stay session { standing = Gone why }

-- | The other side said where the game stands, so that is where it
-- stands.
--
-- It says which colour the player has as well, because in a game
-- somebody else paired there was nothing to decide it here. This
-- arrives before any move does, and a game that has already started
-- takes it again rather than arguing.
began :: Session -> Opening -> Step
began session opening
  | started.turn == opening.yours = stay settled
  | otherwise = case listening session of
    -- The game starts on the other side's move, which is what a
    -- handicap is: Black's stones are down and White has yet to play.
    -- Somebody has to say so, because nothing else will.
    Just them -> telling settled them [Asked]
    Nothing   -> stay settled
 where
  started = gameFrom opening.width opening.stones opening.opens
  settled =
    session { game = started, human = opening.yours, note = Nothing }

-- | A move arrived. It is played here if it is the move this board is
-- waiting for, and dropped if it is not.
--
-- A server says every move, this program's own included, so the same
-- move arrives after it has already gone down here. The colour is what
-- tells the two apart: a move that has been played here already is one
-- whose colour is no longer the colour to play.
theirMove :: Session -> Color -> Move -> Step
theirMove session color move
  | finished session.game = stay session
  | color /= session.game.turn = stay session
  | otherwise = case playMove color move session.game of
    -- What arrived is something this program's rules do not allow,
    -- which means the two boards have drifted apart. Saying so is the
    -- only honest thing left to do.
    Left reason -> stay session { standing = Gone (disagreement reason) }
    Right game
      | finished game, Just them <- listening session ->
        telling session { game = game } them [Counting]
      | otherwise -> stay session { game = game }
 where
  disagreement reason =
    "The other side and the board disagree: " <> describeIllegal reason

-- | Moves came off over there, so they come off here as well.
tookBack :: Session -> Int -> Step
tookBack session count
  | count <= 0 = stay session
  | otherwise  = stay session { game = wind count session.game }
 where
  wind 0 game = game
  wind n game = maybe game (wind (n - 1)) (undoMove game)

-- | How far back to go, and where that lands, taking at most two moves
-- off.
rewind :: Color -> Game -> Maybe (Int, Game)
rewind human = go 0
 where
  go taken game
    | taken > 0 && game.turn == human = Just (taken, game)
    | taken >= 2 = Just (taken, game)
    | otherwise = case undoMove game of
      Nothing     -> if taken > 0 then Just (taken, game) else Nothing
      Just before -> go (taken + 1) before
