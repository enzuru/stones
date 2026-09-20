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
  , canTalk
  , canPlayAgain
  , playingAgain
  , settling
  , deadIn
  , canAccept
  , tick
  , counting
  , clockOf
  , listening
  , opponentOf
  , sessionSize
  )
where

import           Data.Maybe                     ( isJust )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import           GI.Gtk.Declarative.App.Simple  ( Transition(..)
                                                , none
                                                , perform
                                                )
import           Go.Board                       ( group
                                                , stoneAt
                                                )
import           Go.Game
import           Go.Types
import           Stones.Opponent
import           Stones.Setup                   ( Setup(..) )

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
  , setup    :: Setup
    -- ^ What this game was started from, so that it can be started
    -- over with the same answers rather than with the page's.
  , note     :: Maybe Note
    -- ^ Something to say about this game, if there is anything.
  , said     :: [Chat]
    -- ^ What has been said about it, oldest first.
  , clock    :: Maybe Clock
    -- ^ How long each player has left, in a game that is counted.
    -- A game nobody is timing has none, and shows none.
  , dead     :: Maybe [Coord]
    -- ^ In a game that has stopped and is being counted, the stones
    -- both sides are being asked to agree are dead. A game still being
    -- played has none, and that is what tells the two apart.
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
  | AcceptPressed
    -- ^ The player agrees that the stones marked dead are the dead
    -- ones, and the game can be counted from there.
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

-- | A game set up like this, waiting for an opponent.
starting :: Setup -> Session
starting setup = Session { game     = newGame setup.size
                         , standing = Starting
                         , human    = setup.human
                         , setup    = setup
                         , note     = Nothing
                         , said     = []
                         , clock    = Nothing
                         , dead     = Nothing
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

-- | A second going by.
--
-- The server says where the clocks are when a move is made and says
-- nothing in between, so a clock that was only ever set from the
-- server would sit still through the whole of somebody's turn, which
-- is the one time a player is watching it. This counts those seconds.
-- Whatever the server says next is taken over the top, because the
-- server is the one that decides.
--
-- A game that has ended is left alone. Nobody is on the clock in a
-- game that is over, whatever the last thing the server said about it
-- was.
tick :: Session -> Session
tick session
  | finished session.game = session
  | otherwise             = session { clock = ticking <$> session.clock }

-- | Is there a clock going down in this game?
--
-- This is what says whether the seconds are worth counting at all. A
-- game with no clock, or one that has ended, or one that is paused,
-- does not need a stream waking the window once a second.
counting :: Session -> Bool
counting session = case session.clock of
  Just clock | not (finished session.game) -> isJust clock.running
  _ -> False

-- | One player's clock, if the game has one.
clockOf :: Session -> Color -> Maybe Time
clockOf session color = pick <$> session.clock
 where
  pick :: Clock -> Time
  pick clock = case color of
    Black -> clock.black
    White -> clock.white

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
  -- What a click means depends on whether the game is still being
  -- played. Once it has stopped, the board is not taking moves and a
  -- click is about a group rather than about a point.
  Clicked coord -> case session.dead of
    Just marks -> marking session marks coord
    Nothing    -> humanMove session (Play coord)
  Passed        -> humanMove session Pass
  ResignPressed -> resign session
  UndoPressed   -> takeBack session
  Says text     -> says session text
  AcceptPressed -> accepting session
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

-- | Can this game be started over?
--
-- A program on this machine can be stopped and started again whenever
-- anybody likes, and the only thing lost is a game against a program.
-- Somebody at the other end of a server is in the middle of a game,
-- and walking out of one is not starting over, it is leaving. So a
-- game against one of those waits until it has ended.
canPlayAgain :: Session -> Bool
canPlayAgain session = case session.standing of
  -- Nothing to start over. It has not started.
  Starting -> False
  Gone _   -> True
  Here it  -> finished session.game || not it.talks

-- | What to ask for to play this game once more.
--
-- The same answers it was started from, and, when the other side is
-- somebody who can be asked for by name, that name as well. On a
-- server that turns the next game into a rematch against the same
-- player rather than a fresh stranger, which is what the website
-- offers at the end of a game. They may turn it down, and then there
-- is no game, and the tab says so.
playingAgain :: Session -> Setup
playingAgain session = case listening session >>= (.again) of
  Just who -> session.setup { versus = who }
  Nothing  -> session.setup

-- | Is there anybody on the other side to talk to?
--
-- A game against a program is not a conversation. Showing a pane to
-- say something in, beside a board where nothing would ever answer, is
-- worse than showing none.
canTalk :: Session -> Bool
canTalk session = maybe False (.talks) (listening session)

-- * Counting
--------------

-- | Is this game stopped and being counted?
settling :: Session -> Bool
settling session = isJust session.dead

-- | The stones marked dead, which is none at all in a game that is
-- still being played.
deadIn :: Session -> [Coord]
deadIn session = maybe [] id session.dead

-- | Is there a count to agree to, and somebody to agree to it with?
canAccept :: Session -> Bool
canAccept session = settling session && isJust (listening session)

-- | A click while the game is being counted, which is about the group
-- under it rather than about the point.
--
-- The whole group goes at once, because a group lives or dies
-- together: marking one stone of it and leaving the rest is not a
-- thing either player could mean.
--
-- What is marked here is not written down here. The other side says
-- what the marks are and both players see the same ones, the same way
-- both see the same moves. A move goes down before it is said because
-- somebody is waiting to play the next one; nobody is waiting on a
-- mark, so this one waits for the answer and cannot drift.
marking :: Session -> [Coord] -> Coord -> Step
marking session marks coord = case listening session of
  Nothing -> stay session
  Just them -> case stoneAt board coord of
    -- An empty point has no group on it, so there is nothing a click
    -- there could be about.
    Nothing -> stay session
    Just _  -> telling session them [Marking chain (not wasDead)]
 where
  board   = session.game.board
  chain   = Set.toList (group board coord)
  wasDead = coord `elem` marks

-- | The player agrees to the count as it stands.
--
-- The game is counted once both sides have agreed to the same set, and
-- the server is what decides that. Nothing changes here until it says
-- so.
accepting :: Session -> Step
accepting session = case (session.dead, listening session) of
  (Just marks, Just them) -> telling session them [Accepting marks]
  _                       -> stay session

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
  -- A stray press of the return key on an empty line is not something
  -- to say, and saying nothing to everybody is worse than saying
  -- nothing at all.
  Here it | not (Text.null trimmed) ->
    Transition session (perform (saying it [Said trimmed]))
  _ -> stay session
  where trimmed = Text.strip text

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
  Hushed            -> stay session { said = [] }
  Undid count       -> tookBack session count
  Ticked clock      -> stay session { clock = Just clock }
  -- Not about this game. The window takes it off the stream before it
  -- gets here, and a game that is handed one anyway leaves it alone.
  Invited _         -> stay session
  -- The game has stopped. Whatever is already marked stays marked:
  -- this says which part of the game it is and not what is in it, and
  -- the two arrive in whichever order they arrive in.
  Stopped           -> stay session { dead = Just (deadIn session) }
  Removing marks    -> stay session { dead = Just marks }
  -- The counting is over, so there is nothing left to agree about.
  Counted out       ->
    stay session { note = Just (Result out), dead = Nothing }
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
