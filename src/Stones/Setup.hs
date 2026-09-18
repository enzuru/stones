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

import           Data.Text                      ( Text )
import qualified Data.Vector                   as Vector
import qualified Data.Text                     as Text

import qualified GI.Adw                        as Adw
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Adwaita.Bin ( )

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
-- player sees it from inside the program.
launchPage :: Setup -> Widget SetupEvent
launchPage setup = bin
  Adw.StatusPage
  [ #iconName := "com.github.enzuru.Stones"
  , #title := "Stones"
  , #description := "Play Go against GNU Go"
  ]
  (bin
    Adw.Clamp
    [#maximumSize := 340]
    (container
      Gtk.Box
      [#orientation := Gtk.OrientationVertical, #spacing := 18]
      [ BoxChild defaultBoxChildProperties
        $ choice "Board" (map boardChoice boardWidths)
      , BoxChild defaultBoxChildProperties $ choice
        "You play"
        [ pick (colorWord side) (setup.human == side) (ChoseSide side)
        | side <- [Black, White]
        ]
      , BoxChild defaultBoxChildProperties $ choice
        "Opponent"
        [ pick (describeStrength which)
               (setup.strength == which)
               (ChoseStrength which)
        | which <- strengths
        ]
      , BoxChild defaultBoxChildProperties { padding = 6 } $ widget
        Gtk.Button
        [ #label := "Start Game"
        , #halign := Gtk.AlignCenter
        , classes ["suggested-action", "pill"]
        , on #clicked StartPressed
        ]
      ]
    )
  )
 where
  boardChoice n =
    pick (let w = Text.pack (show n) in w <> "\215" <> w)
         (setup.size == n)
         (ChoseBoard n)

-- | One row of the page: what is being chosen, and the buttons to
-- choose from.
choice :: Text -> [Widget SetupEvent] -> Widget SetupEvent
choice label buttons = container
  Gtk.Box
  [#orientation := Gtk.OrientationVertical, #spacing := 6]
  [ BoxChild defaultBoxChildProperties $ widget
    Gtk.Label
    [#label := label, #halign := Gtk.AlignStart, classes ["heading"]]
  , BoxChild defaultBoxChildProperties $ container
    Gtk.Box
    [#homogeneous := True, classes ["linked"]]
    -- The children of a container are a vector, and OverloadedLists
    -- covers a literal and leaves a comprehension alone.
    (Vector.fromList
      [ BoxChild defaultBoxChildProperties { expand = True, fill = True } button
      | button <- buttons
      ]
    )
  ]

-- | One button of a row.
--
-- The one that is chosen carries the accent colour, which is what a
-- row of linked buttons looks like when one of them is picked. Flat
-- buttons were tried instead, to keep the accent for the one button
-- that starts something, and they read as four loose labels rather
-- than as one control.
--
-- They are buttons rather than toggle buttons. A toggle clicked while
-- it is already on turns itself off, and the markup that follows says
-- what it always said, so nothing turns it back on.
pick :: Text -> Bool -> SetupEvent -> Widget SetupEvent
pick label chosen event = widget
  Gtk.Button
  [ #label := label
  , classes (if chosen then ["suggested-action"] else [])
  , on #clicked event
  ]
