-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | The pane beside the board, where the two players talk.
--
-- Only a game with somebody on the other side has one. A program takes
-- what is said to it and says nothing back, so a window playing one
-- shows no pane at all rather than an empty one.
module Stones.Chat
  ( chatPane
  , messagesName
  , scrolledToTheEnd
  )
where

import           Control.Monad                  ( void )
import           Data.Text                      ( Text )
import qualified Data.Vector                   as Vector

import qualified GI.GLib                       as GLib
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative

import           Stones.Session                 ( Chat(..)
                                                , SessionEvent(..)
                                                )

-- | The name the pane's own messages go under, which is what the
-- scrolled window around them is pointed at.
messagesName :: Text
messagesName = "stones-chat"

-- | The pane: what has been said, and a line to say something on.
--
-- What the player types is not written down here. It goes to the other
-- side and comes back, so the same words show whether this program or
-- the other one put them there, and everybody sees them in the order
-- everybody else does.
chatPane :: [Chat] -> Widget SessionEvent
chatPane said = container
  Gtk.Box
  [ #orientation := Gtk.OrientationVertical
  , #widthRequest := 260
  , classes ["background"]
  ]
  [ BoxChild defaultBoxChildProperties { expand = True, fill = True } $ bin
    Gtk.ScrolledWindow
    [ #hscrollbarPolicy := Gtk.PolicyTypeNever
    , #propagateNaturalHeight := True
    , scrolledToTheEnd messagesName
    ]
    (container
      Gtk.Box
      [ #name := messagesName
      , #orientation := Gtk.OrientationVertical
      , #spacing := 12
      , #marginStart := 12
      , #marginEnd := 12
      , #marginTop := 12
      , #marginBottom := 12
      , #valign := Gtk.AlignEnd
      ]
      (Vector.fromList (map lineOf said))
    )
  , BoxChild defaultBoxChildProperties $ widget
    Gtk.Entry
    [ #placeholderText := "Say something"
    , #marginStart := 6
    , #marginEnd := 6
    , #marginTop := 6
    , #marginBottom := 6
    , onM #activate saying
    ]
  ]

-- | One thing somebody said: who said it, and what it was.
--
-- Two labels rather than one line, because the name wants to be
-- quieter than the words and half a label cannot be styled without
-- markup, which would mean escaping whatever anybody typed.
lineOf :: Chat -> BoxChild SessionEvent
lineOf what = BoxChild defaultBoxChildProperties $ container
  Gtk.Box
  [#orientation := Gtk.OrientationVertical, #spacing := 2]
  [ BoxChild defaultBoxChildProperties $ widget
    Gtk.Label
    [#label := what.who, #xalign := 0, classes ["caption", "dim-label"]]
  , BoxChild defaultBoxChildProperties $ widget
    Gtk.Label
    [ #label := what.what
    , #xalign := 0
    , #wrap := True
    , #selectable := True
    ]
  ]

-- | Take what was typed, and leave the line empty for the next thing.
--
-- Whatever was typed goes as it was typed. What is worth saying is the
-- game's rule rather than the entry's: a stray press of the return key
-- on an empty line is dropped there.
saying :: Gtk.Entry -> IO SessionEvent
saying entry = do
  typed <- Gtk.editableGetText entry
  Gtk.editableSetText entry ""
  pure (Says typed)

-- | Keep the newest message in view.
--
-- A chat showing the oldest message is a chat nobody can read, and
-- there is no declarative way to say "and scroll to the end of it". A
-- reference is the one thing that runs after every patch, so the
-- scrolled window is pointed at the messages it is showing and scrolls
-- to the end of them.
--
-- Not straight away. References are resolved at the ordinary idle
-- priority, which is above the one GTK lays widgets out at, so at that
-- moment the pane is still the height it was before the new message
-- went in and the end of it is a message short. Waiting for a lower
-- priority is waiting for the layout.
scrolledToTheEnd :: Text -> Attribute Gtk.ScrolledWindow event
scrolledToTheEnd = reference $ \window _ ->
  void . GLib.idleAdd GLib.PRIORITY_LOW $ do
    adjustment <- Gtk.scrolledWindowGetVadjustment window
    upper      <- Gtk.adjustmentGetUpper adjustment
    showing    <- Gtk.adjustmentGetPageSize adjustment
    Gtk.adjustmentSetValue adjustment (max 0 (upper - showing))
    pure False
