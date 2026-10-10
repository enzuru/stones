-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE NoFieldSelectors      #-}

-- | The other side of a game, whatever is on it.
--
-- GNU Go answers when it is asked and says nothing otherwise. A game
-- on a server answers when it likes, and says things nobody asked
-- about: the other player moved, or wrote something, or went away. A
-- seam shaped as a question and an answer fits the first and not the
-- second.
--
-- So this one has two halves that do not line up. What this program
-- does is 'Told' to the other side, one thing at a time. What arrives
-- is 'Heard', whether anybody asked for it or not. An opponent that
-- only answers questions, like GNU Go, does its answering by putting
-- the answer on the stream, so the program above cannot tell the two
-- kinds apart and does not have to.
module Stones.Opponent
  ( Opponent(..)
  , Opponents(..)
  , choosing
  , Told(..)
  , Heard(..)
  , Opening(..)
  , Invitation(..)
  , Ongoing(..)
  , Clock(..)
  , Time(..)
  , ticking
  , describeTime
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Pipes                          ( Producer )

import           Go.Types
import           Stones.Setup                   ( Against(..)
                                                , Ongoing(..)
                                                , Setup(..)
                                                )

-- | The other side of one game.
data Opponent = Opponent
  { name  :: Text
    -- ^ What to call it in the window.
  , tell  :: Told -> IO (Either Text ())
    -- ^ Say one thing to it. The answer says whether it was said, and
    -- not what came of it: what comes of it arrives on 'hear', because
    -- with a server that is where it would arrive anyway.
  , hear  :: Producer Heard IO ()
    -- ^ Everything that arrives. This runs for as long as the game is
    -- open, and a game that ends is a producer that ends.
  , leave :: IO ()
    -- ^ Let it go. This does not fail: there is nothing the window
    -- would do about it.
  , talks :: Bool
    -- ^ Whether there is anybody on the other side to talk to. A
    -- program takes what is said to it and has nothing to say back, so
    -- a window playing one shows no pane to say it in.
  , again :: Maybe Text
    -- ^ What to ask for to play this same opponent once more, if they
    -- are somebody who can be asked for.
    --
    -- A program on this machine is nobody in particular: the next one
    -- started is the same one, so there is nothing to ask for and this
    -- is nothing. Somebody on a server has a name, and asking for them
    -- by it is a rematch, which they may turn down.
  }

-- | Where a game gets an opponent, and where it gives one back.
data Opponents = Opponents
  { open  :: Setup -> IO (Either Text Opponent)
    -- ^ An opponent for a game set up like this.
  , close :: Opponent -> IO ()
    -- ^ Let one go, when the game it was playing has closed.
  , take' :: Invitation -> IO (Either Text Opponent)
    -- ^ Take a game somebody else offered, rather than asking for one.
  , ongoing :: IO (Either Text [Ongoing])
    -- ^ The games this player is already in, wherever they were
    -- started. A place with no account has nobody to be in any.
  , resume :: Ongoing -> IO (Either Text Opponent)
    -- ^ Open one of those, which is joining a game rather than
    -- starting or accepting one.
  , enter :: IO (Either Text Text)
    -- ^ Sign in wherever the games are, answering with what to call
    -- the player. Only one of the two places asks for it, and the
    -- window only asks when that is the place chosen.
  }

-- | One place to get a game from, made of two, picked by where the
-- game says it is played.
--
-- Letting one go is the same either way: an opponent knows how to
-- leave, and neither place keeps anything about it that outliving the
-- leaving would matter to.
choosing :: Opponents -> Opponents -> Opponents
choosing computer online = Opponents
  { open  = \setup -> case setup.against of
              Computer -> computer.open setup
              Online   -> online.open setup
  , close = (.leave)
    -- Only one of the two places has anybody in it to offer a game,
    -- or any games to already be in.
  , take'   = online.take'
  , ongoing = online.ongoing
  , resume  = online.resume
  , enter   = online.enter
  }

-- | A game somebody has offered to play.
--
-- It arrives out of nowhere: not an answer to anything this program
-- asked for, and not about any game it is already playing. Somebody
-- somewhere picked this player and said they would like a game.
data Invitation = Invitation
  { from   :: Text
    -- ^ Who is offering.
  , about  :: Setup
    -- ^ What they are offering, as far as it can be said in the terms
    -- the rest of this program uses.
  , token  :: Int
    -- ^ What to answer with, which is the offer's own number rather
    -- than anything about the game.
  , number :: Int
    -- ^ The game it would be. The server picks one when the offer is
    -- made, before anybody has said yes.
  }
  deriving (Eq, Show)

-- | What this program does, which the other side is told about.
--
-- The rules live in this program rather than in the opponent, so what
-- follows a move is something this program knows and the other side
-- may not. That is why a move and the turn that follows it are two
-- things rather than one: an opponent is told what happened, and then
-- told whether anybody is to move next.
data Told
  = Moved Move
    -- ^ A move was made on this board.
  | TookBack Int
    -- ^ This many moves came off this board.
  | Asked
    -- ^ It is the other side's turn.
  | Resigned
    -- ^ The player gave the game up.
  | Counting
    -- ^ Nobody is to move, because the game has ended. What is the
    -- score?
  | Marking [Coord] Bool
    -- ^ These stones are dead, or are alive after all. At the end of a
    -- game the two players agree which stones would have been taken if
    -- play had gone on, because nobody plays those moves out.
  | Accepting [Coord]
    -- ^ This is the set of dead stones, as far as this player is
    -- concerned. The game is counted once both sides have said so
    -- about the same set.
  | Said Text
    -- ^ Something to say to whoever is listening.
  deriving (Eq, Show)

-- | The position a game starts from, which is not always an empty
-- board.
--
-- A handicap game starts with Black's stones already down and White to
-- play. A game being rejoined starts wherever it had got to. Both are
-- this, and the side that knows is the other one.
data Opening = Opening
  { width  :: Int
  , stones :: [(Color, Coord)]
    -- ^ What is already on the board.
  , opens  :: Color
    -- ^ Who plays next from here.
  , yours  :: Color
    -- ^ The colour the player at this window has in this game, which
    -- is something only the other side knows when it did the pairing.
  , marked :: Maybe [Coord]
    -- ^ The stones marked dead, in a game that has stopped and is
    -- being counted. A game still being played has none, and that is
    -- what tells the two apart.
    --
    -- A game is not always joined while it is still being played. One
    -- picked out of the games this player is already in can be
    -- anywhere, the counting at the end included, and a board handed
    -- over as playable then is a board that takes clicks the server
    -- will refuse.
  }
  deriving (Eq, Show)

-- | How long each player has left.
data Clock = Clock
  { black   :: Time
  , white   :: Time
  , running :: Maybe Color
    -- ^ Whose is going down. Nobody's before the game starts and after
    -- it ends.
  }
  deriving (Eq, Show)

-- | One player's clock.
--
-- A game is usually counted in two parts: the time to think in, and
-- then a number of shorter periods, each of which is given back if the
-- player moves inside it. What is left of whichever is running now is
-- the first of these, and how many are still to come is the second.
data Time = Time
  { seconds :: Int
  , periods :: Int
    -- ^ How many short periods are still to come, or none.
  , each    :: Int
    -- ^ How long one of those is.
  }
  deriving (Eq, Show)

-- | A second going by.
--
-- The server says where a clock is when something happens, and says
-- nothing in between, so the seconds in between are counted here. What
-- the server says next is taken over this, because the server is the
-- one that decides.
ticking :: Clock -> Clock
ticking clock = Clock { black   = down Black clock.black
                      , white   = down White clock.white
                      , running = clock.running
                      }
 where
  down side time
    | clock.running /= Just side = time
    | time.seconds > 0           = time { seconds = time.seconds - 1 }
    | time.periods > 0           = time { seconds = max 0 (time.each - 1)
                                        , periods = time.periods - 1
                                        }
    | otherwise                  = time

-- | A clock as a window shows one.
describeTime :: Time -> Text
describeTime time = clock <> extra
 where
  clock =
    Text.pack (show minutes) <> ":" <> Text.justifyRight 2 '0' (Text.pack (show rest))
  (minutes, rest) = time.seconds `divMod` 60
  extra | time.periods > 0 = " (\215" <> Text.pack (show time.periods) <> ")"
        | otherwise        = ""

-- | What arrives from the other side.
data Heard
  = Began Opening
    -- ^ This is the game, and this is where it stands. It arrives
    -- before anything else and can arrive again.
  | Played Color Move
    -- ^ This was played, by this colour. The colour is said rather
    -- than assumed, because a server says it and is the one that
    -- knows.
  | Spoke Text Text
    -- ^ Who said it, and what they said.
  | Undid Int
    -- ^ This many moves came off over there.
  | Hushed
    -- ^ Forget what has been said about this game. A server says this
    -- before it says the whole conversation over again, which it does
    -- when a game ends.
  | Stopped
    -- ^ Nobody moves again. What is left is agreeing which stones are
    -- dead, and the game is counted from there.
  | Removing [Coord]
    -- ^ The stones marked dead, as they now stand. The whole set every
    -- time, rather than what changed, so a window that missed one is
    -- not left showing a board nobody else can see.
  | Invited Invitation
    -- ^ Somebody has offered a game. It is not about the game it
    -- arrives on: a connection carries everything said to this player,
    -- and an offer is one of those things.
  | Ticked Clock
    -- ^ Where the clocks are. It arrives when something happens and
    -- not in between.
  | Counted Text
    -- ^ What the game came to.
  | Lost Text
    -- ^ The other side has gone, and this is why. Nothing follows it.
  deriving (Eq, Show)
