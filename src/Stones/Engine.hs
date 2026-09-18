-- | What the program needs from an opponent.
--
-- Everything the application does to an opponent is one of the seven
-- actions below, so an opponent is a record of them rather than a
-- class. GNU Go is one, in "Stones.Engine.GnuGo". A game on a server
-- is the next one, and it fits the same shape: a move is sent, a move
-- comes back, and the board says what the score was.
--
-- Every action that can fail answers with 'Either', and the left side
-- is what to show the player. An opponent that is a process on this
-- machine and an opponent that is a connection over the network fail
-- in different ways, and the application does the same thing with
-- both: it stops asking and it says what happened.
module Stones.Engine
  ( Engine(..)
  )
where

import           Data.Text                      ( Text )

import           Go.Types

-- | An opponent, and the handful of things that can be asked of one.
data Engine = Engine
  { engineName    :: Text
    -- ^ What to call this opponent in the window.
  , engineNewGame :: Int -> IO (Either Text ())
    -- ^ Clear the board and play on one of this width from now on.
  , engineNotify  :: Color -> Move -> IO (Either Text ())
    -- ^ Tell the opponent about a move somebody else made.
  , engineGenMove :: Color -> IO (Either Text Move)
    -- ^ Ask the opponent for a move of its own, which it also plays.
  , engineUndo    :: Int -> IO (Either Text ())
    -- ^ Take this many moves back off the opponent's board.
  , engineScore   :: IO (Either Text Text)
    -- ^ What the opponent makes the score, once the game has ended.
  , engineClose   :: IO ()
    -- ^ Let the opponent go. This does not fail: there is nothing the
    -- application would do about it.
  }
