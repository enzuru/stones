-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | What a game is started from, and the page that asks for it.
--
-- A tab opens on this page rather than on a board. The program has
-- nothing to go on when it starts: which board, which colour, and how
-- hard the opponent should try are three things only the player knows,
-- and starting a game before asking is guessing at all three.
--
-- Nothing is started while this page is up. The opponent is a process,
-- and it is not worth one until somebody has asked for a game.
module Stones.Setup
  ( Setup(..)
  , SetupEvent(..)
  , defaultSetup
  , boardWidths
  , chose
  , describeSetup
  , launchPage
  )
where

import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text )
import qualified Data.Vector                   as Vector
import qualified Data.Text                     as Text

import qualified GI.Adw                        as Adw
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Adwaita.Bin ( )
import           GI.Gtk.Declarative.Adwaita.Rows
                                                ( rowSuffix )
import           GI.Gtk.Declarative.Adwaita.ToggleGroup
                                                ( ToggleGroupParams(..)
                                                , defaultToggleGroupParams
                                                , toggle
                                                , toggleGroup
                                                )

import           Go.Types
import           Stones.Engine                  ( Strength(..)
                                                , describeStrength
                                                , strengths
                                                )

-- | What a game will be started with.
data Setup = Setup
  { size     :: Int
    -- ^ The width of the board.
  , human    :: Color
    -- ^ The colour the player takes.
  , strength :: Strength
    -- ^ How hard the opponent is asked to try.
  }
  deriving (Eq, Show)

-- | A 19x19 game as Black against an opponent trying hard, which is
-- what a game of Go is when nobody says otherwise.
defaultSetup :: Setup
defaultSetup = Setup { size = 19, human = Black, strength = Fierce }

-- | The boards on offer. Every board a player is likely to want, and
-- no more: the rules work on any width, and nobody plays 14x14.
boardWidths :: [Int]
boardWidths = [9, 13, 19]

-- | What the player did to the page.
data SetupEvent
  = ChoseBoard Int
  | ChoseSide Color
  | ChoseStrength Strength
  | StartPressed
  deriving (Eq, Show)

-- | What a choice does to the page. Starting is not a choice, and the
-- window is what acts on it.
chose :: Setup -> SetupEvent -> Setup
chose setup = \case
  ChoseBoard n        -> setup { size = n }
  ChoseSide side      -> setup { human = side }
  ChoseStrength which -> setup { strength = which }
  StartPressed        -> setup

-- | The line under the title while this page is up.
describeSetup :: Setup -> Text
describeSetup setup =
  board
    <> "  \183  "
    <> colorWord setup.human
    <> "  \183  "
    <> describeStrength setup.strength
  where board = let n = Text.pack (show setup.size) in n <> "\215" <> n

-- | The name of a colour, for a sentence.
colorWord :: Color -> Text
colorWord Black = "Black"
colorWord White = "White"

-- * The page
-------------

-- | The page a tab opens on.
--
-- An @AdwStatusPage@ is the widget for a window with nothing in it
-- yet. It carries the program's own icon, which is the one place a
-- player sees it from inside the program. Under it is a settings page
-- of the ordinary kind: a preferences group, a row per thing to
-- choose, and a toggle group in each row.
launchPage :: Setup -> Widget SetupEvent
launchPage setup = bin
  Adw.StatusPage
  [ #iconName := "com.github.enzuru.Stones"
  , #title := "Stones"
  , #description := "Play Go against GNU Go"
  ]
  (bin
    Adw.Clamp
    [#maximumSize := 400]
    (container
      Gtk.Box
      [#orientation := Gtk.OrientationVertical, #spacing := 24]
      [ BoxChild defaultBoxChildProperties $ container
        Adw.PreferencesGroup
        []
        [ choice "Board"
                 [ (Text.pack (show n), boardLabel n, n) | n <- boardWidths ]
                 setup.size
                 ChoseBoard
        , choice "You play"
                 [ (colorWord side, colorWord side, side)
                 | side <- [Black, White]
                 ]
                 setup.human
                 ChoseSide
        , choice
          "Opponent"
          [ (describeStrength which, describeStrength which, which)
          | which <- strengths
          ]
          setup.strength
          ChoseStrength
        ]
      , BoxChild defaultBoxChildProperties $ widget
        Gtk.Button
        [ #label := "Start Game"
        , #halign := Gtk.AlignCenter
        , classes ["suggested-action", "pill"]
        , on #clicked StartPressed
        ]
      ]
    )
  )
  where boardLabel n = let w = Text.pack (show n) in w <> "\215" <> w

-- | One row of the page: what is being chosen, and a group of toggles
-- to choose from.
--
-- Each toggle carries a name, which is what the group answers with, so
-- the names and what they stand for are given together here and looked
-- up on the way back.
choice
  :: Eq a
  => Text
  -> [(Text, Text, a)]
  -> a
  -> (a -> SetupEvent)
  -> Widget SetupEvent
choice title options chosen report = container
  Adw.ActionRow
  [#title := title]
  [ rowSuffix
      (toggleGroup
        [#valign := Gtk.AlignCenter]
        defaultToggleGroupParams
          { toggles     = Vector.fromList
            [ toggle name label | (name, label, _) <- options ]
          , active      = listToMaybe
            [ name | (name, _, value) <- options, value == chosen ]
          , onActivated = Just (report . meaning)
          }
      )
  ]
 where
  -- A group answers with one of the names it was given, so the other
  -- way is not a way this goes. What is chosen now is what it answers
  -- with if it ever did, which is a choice that changes nothing.
  meaning name =
    fromMaybe chosen (lookup name [ (n, value) | (n, _, value) <- options ])
