{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | The window and what it does.
--
-- The program keeps the rules itself, in "Go.Game", and also tells the
-- engine about every move. Both boards therefore hold the same
-- position, and the reason for keeping two is that the rules answer a
-- click at once: a stone lands under the pointer without waiting for
-- a process to think. The engine is asked for a move and for a score,
-- and for nothing else.
--
-- If the two ever disagree, which would mean a bug in one of them, the
-- game stops and says so rather than playing on from a position only
-- half of the program believes in.
module Stones.App
  ( State(..)
  , Event(..)
  , Reply(..)
  , startingState
  , update'
  , view'
  , application
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text

import qualified GI.Adw                        as Adw
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Adwaita.Bin ( )
import           GI.Gtk.Declarative.Adwaita.HeaderBar
                                                ( headerBarEnd
                                                , headerBarStart
                                                )
import           GI.Gtk.Declarative.Adwaita.Slots
                                                ( titleWidget )
import           GI.Gtk.Declarative.Adwaita.ToolbarView
                                                ( toolbarBottom
                                                , toolbarContent
                                                , toolbarTop
                                                )
import           GI.Gtk.Declarative.App.Simple
import qualified Pipes

import           Go.Game
import           Go.Types
import           Stones.Engine
import           Stones.Goban

-- | Everything the window shows.
data State = State
  { stateGame     :: Game
    -- ^ The position, and whose turn it is.
  , stateEngine   :: Engine
    -- ^ The opponent.
  , stateHuman    :: Color
    -- ^ The colour the player at this window has.
  , stateThinking :: Bool
    -- ^ Whether the engine has been asked for something and has not
    -- answered yet, which is when the board takes no clicks.
  , stateMessage  :: Text
    -- ^ The line under the title.
  , stateBroken   :: Bool
    -- ^ Whether the engine has failed. A broken game takes no more
    -- moves, because there is nothing left to play against.
  , statePending  :: Maybe Int
    -- ^ A board width waiting for the engine to be free.
    --
    -- Only one thing is ever asked of the engine at a time. A command
    -- is several lines of protocol, and a second conversation started
    -- in the middle of the first would have its lines read as answers
    -- to the wrong questions. So a new game asked for while the engine
    -- is thinking waits here until the answer comes back.
  }

-- | What the window reports.
data Event
  = Clicked Coord
    -- ^ The player clicked a point on the board.
  | Passed
  | ResignPressed
  | UndoPressed
  | NewGamePressed Int
  | FromEngine Reply
    -- ^ The engine answered what it was last asked.
  | Closed
  deriving (Show)

-- | What the engine answered.
data Reply
  = Moved Move
    -- ^ It played this.
  | Ready
    -- ^ It did as it was told and has nothing to say: a board was set
    -- up, or moves were taken off one.
  | Scored Text
    -- ^ This is what it makes the score.
  | Failed Text
    -- ^ It could not do as it was told.
  deriving (Show)

-- | A window with a new game in it.
--
-- The game is not set up here. The application sends itself a
-- 'NewGamePressed' as its first event, so that setting up the first
-- board and setting up every later one are the same piece of code, and
-- so that the window is on the screen while the engine is starting.
--
-- Nothing has been asked of the engine yet, so this state is not
-- thinking. Saying that it was would make the first event queue behind
-- an answer that nobody was waiting for, and the board would never be
-- set up at all.
startingState :: Engine -> Color -> Int -> State
startingState engine human n = State { stateGame     = newGame n
                                     , stateEngine   = engine
                                     , stateHuman    = human
                                     , stateThinking = False
                                     , stateMessage  = "Setting up the board."
                                     , stateBroken   = False
                                     , statePending  = Nothing
                                     }

-- * Updating
-------------

update' :: State -> Event -> Transition State Event
update' state = \case
  Closed        -> Exit

  Clicked coord -> humanMove state (Play coord)
  Passed        -> humanMove state Pass

  -- A resignation ends the game where it stands. There is nothing to
  -- tell the engine: the protocol has no command for it, and there is
  -- no next move to ask for.
  ResignPressed
    | not (yourTurn state) -> Transition state none
    | otherwise -> case playMove (stateHuman state) Resign (stateGame state) of
      Left  _    -> Transition state none
      Right game -> Transition
        state { stateGame     = game
              , stateThinking = False
              , stateMessage  = "You resigned. "
                                  <> colorWord (opposite (stateHuman state))
                                  <> " wins."
              }
        none

  UndoPressed -> takeBack state

  -- Nothing is asked of the engine while it is answering something
  -- else, so a new game asked for now waits until it has.
  NewGamePressed n
    | stateThinking state -> Transition
      state { statePending = Just n
            , stateMessage = "The new board is waiting for the engine."
            }
      none
    | otherwise -> startGame state n

  FromEngine reply -> case (statePending state, reply) of
    -- The engine is free again and a new game was waiting for it.
    (Just _, Failed why) -> Transition
      state { stateThinking = False
            , stateBroken   = True
            , statePending  = Nothing
            , stateMessage  = why
            }
      none
    (Just n, _) -> startGame state { statePending = Nothing } n
    (Nothing, _) -> engineReply state reply

-- | What one answer from the engine does to the window.
engineReply :: State -> Reply -> Transition State Event
engineReply state = \case
  Moved move -> engineMove state move

  Ready      -> Transition
    state { stateThinking = False, stateMessage = waitingOn state }
    none

  Scored result -> Transition
    state { stateThinking = False, stateMessage = "Result: " <> result }
    none

  Failed why -> Transition
    state { stateThinking = False, stateBroken = True, stateMessage = why }
    none

-- | What to say when the engine has finished and the game is back in
-- the player's hands.
waitingOn :: State -> Text
waitingOn state
  | finished (stateGame state)                    = "The game is over."
  | gameTurn (stateGame state) == stateHuman state = "Your move."
  | otherwise                                     = "The engine is thinking."

-- | The player's own move.
humanMove :: State -> Move -> Transition State Event
humanMove state move
  | not (yourTurn state) = Transition state none
  | otherwise = case playMove (stateHuman state) move (stateGame state) of
    Left reason -> Transition state { stateMessage = describeIllegal reason } none
    Right game
      | finished game -> Transition
        state { stateGame     = game
              , stateThinking = True
              , stateMessage  = "Both players passed. Counting."
              }
        (tellAndScore state move)
      | otherwise -> Transition
        state { stateGame     = game
              , stateThinking = True
              , stateMessage  = "The engine is thinking."
              }
        (tellAndAsk state move)

-- | The engine's answer, which is played on this program's board too.
engineMove :: State -> Move -> Transition State Event
engineMove state move = case move of
  Resign -> Transition
    state { stateGame     = resignedGame
          , stateThinking = False
          , stateMessage  = "The engine resigned. You win."
          }
    none
  _ -> case playMove (opposite (stateHuman state)) move (stateGame state) of
    -- The engine has answered with something this program's rules do
    -- not allow, which means the two boards have drifted apart. Saying
    -- so is the only honest thing left to do.
    Left reason -> Transition
      state { stateThinking = False
            , stateBroken   = True
            , stateMessage  = "The engine and the board disagree: "
                                <> describeIllegal reason
            }
      none
    Right game
      | finished game -> Transition
        state { stateGame     = game
              , stateThinking = True
              , stateMessage  = "Both players passed. Counting."
              }
        (askScore state)
      | otherwise -> Transition
        state { stateGame     = game
              , stateThinking = False
              , stateMessage  = if move == Pass
                                  then "The engine passed. Your move."
                                  else "Your move."
              }
        none
 where
  resignedGame = case
      playMove (opposite (stateHuman state)) Resign (stateGame state)
    of
      Right game -> game
      Left  _    -> stateGame state

-- | Take back moves until it is the player's turn again, on both
-- boards.
--
-- One move is not enough: taking back only the engine's answer would
-- leave the player looking at their own move with the engine about to
-- answer it again. Two is the usual number, and one is what is there
-- to take back when the engine opened the game.
takeBack :: State -> Transition State Event
takeBack state
  | stateThinking state = Transition state none
  | otherwise = case rewind (stateHuman state) (stateGame state) of
    Nothing -> Transition
      state { stateMessage = "There is nothing to take back." }
      none
    Just (count, game) -> Transition
      state { stateGame     = game
            , stateThinking = True
            , stateMessage  = "Took back "
                                <> Text.pack (show count)
                                <> (if count == 1 then " move." else " moves.")
            , stateBroken   = False
            }
      (fromEngine (undoThen state game count))

-- | Take the engine back the same number of moves, and ask it to move
-- if the board it is left with is one where it has the turn.
undoThen :: State -> Game -> Int -> IO Reply
undoThen state game count = do
  let engine = stateEngine state
  engineUndo engine count >>= \case
    Left  why -> pure (Failed why)
    Right ()
      | gameTurn game == stateHuman state -> pure Ready
      | otherwise -> either Failed Moved <$> engineGenMove engine (gameTurn game)

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

-- | Start a new game on a board of this width, on both boards.
startGame :: State -> Int -> Transition State Event
startGame state n = Transition
  state { stateGame     = newGame n
        , stateThinking = True
        , stateMessage  = "Setting up a "
                            <> Text.pack (show n)
                            <> "x"
                            <> Text.pack (show n)
                            <> " board."
        , stateBroken   = False
        }
  (fromEngine (setUp (stateEngine state)))
 where
  human = stateHuman state
  setUp engine = engineNewGame engine n >>= \case
    Left  why -> pure (Failed why)
    Right ()  -> if human == Black
      then pure Ready
      -- The player has White, so the engine opens and its move is what
      -- ends the setting up.
      else either Failed Moved <$> engineGenMove engine (opposite human)

-- | Tell the engine what was played, then ask it for its own move.
tellAndAsk :: State -> Move -> Cmd Event
tellAndAsk state move = fromEngine $ do
  let engine = stateEngine state
      human  = stateHuman state
  engineNotify engine human move >>= \case
    Left  why -> pure (Failed why)
    Right ()  -> either Failed Moved <$> engineGenMove engine (opposite human)

-- | Tell the engine what was played, then ask it what the score is.
-- This is the second pass of a game that has just ended.
tellAndScore :: State -> Move -> Cmd Event
tellAndScore state move = fromEngine $ do
  let engine = stateEngine state
  engineNotify engine (stateHuman state) move >>= \case
    Left  why -> pure (Failed why)
    Right ()  -> either Failed Scored <$> engineScore engine

-- | Ask the engine what the score is.
askScore :: State -> Cmd Event
askScore state =
  fromEngine (either Failed Scored <$> engineScore (stateEngine state))

-- | Put a job to the engine, and take its answer as an event.
--
-- The job is not named, so nothing stops it part way through. A
-- command is several lines of protocol, and a job stopped between two
-- of them would leave the answer to the first sitting in the pipe,
-- where it would be read as the answer to whatever was asked next.
fromEngine :: IO Reply -> Cmd Event
fromEngine job = perform (Just . FromEngine <$> job)

-- | Can the player move right now?
yourTurn :: State -> Bool
yourTurn State {..} =
  not stateThinking
    && not stateBroken
    && not (finished stateGame)
    && gameTurn stateGame
    == stateHuman

-- * The window
---------------

view' :: State -> AppView Adw.ApplicationWindow Event
view' state =
  bin
      Adw.ApplicationWindow
      [ #title := "Stones"
      , #defaultWidth := 760
      , #defaultHeight := 820
      , on #closeRequest (True, Closed)
      ]
    $ container
        Adw.ToolbarView
        []
        [ toolbarTop (header state)
        , toolbarContent (board state)
        , toolbarBottom (footer state)
        ]

-- | The bar at the top: a menu that starts a game, the title, and what
-- the engine is called.
header :: State -> Widget Event
header state = container
  Adw.HeaderBar
  [ titleWidget
      (widget
        Adw.WindowTitle
        [#title := "Stones", #subtitle := stateMessage state]
      )
  ]
  [ headerBarStart
    (menuButton
      [#label := "New Game", #tooltipText := "Start a game on a new board"]
      [ menuSection
          Nothing
          [ menuItem "9x9"   (NewGamePressed 9)
          , menuItem "13x13" (NewGamePressed 13)
          , menuItem "19x19" (NewGamePressed 19)
          ]
      ]
    )
  , headerBarEnd
    (widget
      Gtk.Label
      [ #label := engineName (stateEngine state)
      , classes ["dim-label"]
      , #tooltipText := "The program you are playing against"
      ]
    )
  ]

-- | The board itself.
board :: State -> Widget Event
board state = widgetOf (goban [] props)
 where
  game = stateGame state
  props = GobanProps
    { gobanBoard       = gameBoard game
    , gobanLast        = gameLast game
    , gobanHover       = if yourTurn state then Just (stateHuman state) else Nothing
    , gobanCoordinates = True
    }
  widgetOf = fmap (\(GobanClicked coord) -> Clicked coord)

-- | The bar at the bottom: what has been taken on the left, whose turn
-- it is in the middle, and what the player can do on the right.
footer :: State -> Widget Event
footer state = centerBox
  [classes ["toolbar"], #marginStart := 6, #marginEnd := 6]
  (widget Gtk.Label [#label := capturesLine state, classes ["dim-label"]])
  (widget Gtk.Label [#label := turnLine state])
  (container
    Gtk.Box
    [#spacing := 6]
    [ BoxChild defaultBoxChildProperties $ button
      "Pass"
      "Give the move to the other player"
      (yourTurn state)
      Passed
    , BoxChild defaultBoxChildProperties $ button
      "Undo"
      "Take back your last move and the answer to it"
      (not (stateThinking state) && gameMoves (stateGame state) /= [])
      UndoPressed
    , BoxChild defaultBoxChildProperties $ button
      "Resign"
      "Give the game up"
      (yourTurn state)
      ResignPressed
    ]
  )

-- | One button of the bottom bar.
button :: Text -> Text -> Bool -> Event -> Widget Event
button label tooltip enabled event = widget
  Gtk.Button
  [ #label := label
  , #tooltipText := tooltip
  , #sensitive := enabled
  , on #clicked event
  ]

-- | How many stones each player has taken.
capturesLine :: State -> Text
capturesLine state =
  "Black has taken "
    <> Text.pack (show (blackCaptured captures))
    <> ", White has taken "
    <> Text.pack (show (whiteCaptured captures))
  where captures = gameCaptures (stateGame state)

-- | Whose turn it is, or how the game ended.
turnLine :: State -> Text
turnLine state
  | stateBroken state = "The game has stopped."
  | finished game = "The game is over."
  | stateThinking state = "Thinking..."
  | gameTurn game == stateHuman state = "Your move (" <> colorWord (stateHuman state) <> ")"
  | otherwise = "The engine's move"
  where game = stateGame state

-- | The name of a colour, for a sentence.
colorWord :: Color -> Text
colorWord Black = "Black"
colorWord White = "White"

-- | The application, ready to run.
application :: Engine -> Color -> Int -> App Adw.ApplicationWindow State Event
application engine human n = defaultApp
  { update       = update'
  , view         = view'
  , initialState = startingState engine human n
    -- The one event the application sends itself, which sets the first
    -- board up.
  , inputs       = [Pipes.yield (NewGamePressed n)]
  }
