-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

-- | The menu in the corner of the header bar.
--
-- The menu itself is in @data\/ui\/menu.blp@, and what is here is the
-- button that shows it and the actions its items name. A menu is data
-- rather than widgets: it is a @GMenuModel@, which GTK builds from
-- markup and which has no widget in it at all. Writing it in markup is
-- what the format is for, and it is where a translator looks for the
-- words.
--
-- The actions go in a group on the button rather than on the
-- application, so they reach no further than the window they belong
-- to, and so that whether a game can be resigned is a property of this
-- widget rather than of the program.
module Stones.Menu
  ( MenuProps(..)
  , MenuEvent(..)
  , Choice(..)
  , choices
  , mainMenu
  , menuFile
  )
where

import           Control.Exception              ( SomeException
                                                , try
                                                )
import           Data.Foldable                  ( for_ )
import           Data.IORef
import qualified Data.Text                     as Text
import           Data.Vector                    ( Vector )

import           Foreign.Ptr                    ( nullPtr )
import qualified GI.GLib                       as GLib
import qualified GI.Gio                        as Gio
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource ( fromCancellation )

import           Stones.Files                   ( dataFile )

-- | What the menu shows.
newtype MenuProps = MenuProps
  { canResign :: Bool
    -- ^ Whether the game showing is one that can be given up. An item
    -- that cannot be used is greyed rather than gone, so the menu is
    -- the same menu every time it is opened.
  }
  deriving (Eq, Show)

-- | What the menu reports.
data MenuEvent
  = NewGameChosen
  | ResignChosen
  deriving (Eq, Show)

-- | One thing the menu offers: the name its item in the markup uses,
-- what choosing it reports, and whether it can be chosen.
data Choice = Choice
  { name    :: Text.Text
  , reports :: MenuEvent
  , usable  :: Bool
  }
  deriving (Eq, Show)

-- | What the menu offers, for the game it is about.
--
-- An item that cannot be used is greyed rather than gone, so the menu
-- is the same menu every time it is opened. This is the whole of what
-- the widget below decides, which is why it is a function of its
-- argument and not something to be read off a widget.
choices :: MenuProps -> [Choice]
choices props =
  [ Choice { name = "new-game", reports = NewGameChosen, usable = True }
  , Choice { name = "resign", reports = ResignChosen, usable = props.canResign }
  ]

-- | What the widget keeps between patches.
data MenuState = MenuState
  { made     :: [(Text.Text, Gio.SimpleAction)]
  , listener :: IORef (Maybe (MenuEvent -> IO ()))
  }

-- | The file the menu is read from, under the data directory.
menuFile :: FilePath
menuFile = "ui/menu.ui"

-- | The prefix the menu's actions are named under, which is what the
-- items in the markup say.
prefix :: Text.Text
prefix = "window"

-- | The button in the corner, and the menu behind it.
mainMenu
  :: Vector (Attribute Gtk.MenuButton MenuEvent)
  -> MenuProps
  -> Widget MenuEvent
mainMenu customAttributes customParams = Widget CustomWidget { .. }
 where
  customWidget = Gtk.MenuButton

  customCreate :: MenuProps -> IO (Gtk.MenuButton, MenuState)
  customCreate props = do
    button <- Gtk.new Gtk.MenuButton []
    -- The actions are made and connected once, here, and subscribing
    -- only says where their events go.
    sendTo <- newIORef Nothing
    group  <- Gio.simpleActionGroupNew
    actions <- traverse (action group sendTo) (choices props)
    Gtk.widgetInsertActionGroup button prefix (Just group)
    loadMenu button
    pure (button, MenuState { made = actions, listener = sendTo })

  customPatch
    :: MenuProps
    -> MenuProps
    -> MenuState
    -> CustomPatch Gtk.MenuButton MenuState
  customPatch old new state
    | old == new = CustomKeep
    | otherwise = CustomModify $ \_button -> do
      for_ (choices new) $ \choice ->
        for_ (lookup choice.name state.made)
             (`Gio.simpleActionSetEnabled` choice.usable)
      pure state

  customSubscribe _props MenuState { listener } _button callback = do
    writeIORef listener (Just callback)
    pure (fromCancellation (writeIORef listener Nothing))

-- | One action of the group, which reports its choice when it is used.
action
  :: Gio.SimpleActionGroup
  -> IORef (Maybe (MenuEvent -> IO ()))
  -> Choice
  -> IO (Text.Text, Gio.SimpleAction)
action group listener choice = do
  it <- Gio.simpleActionNew choice.name Nothing
  _  <- Gio.onSimpleActionActivate it $ \_parameter -> do
    heard <- readIORef listener
    for_ heard ($ choice.reports)
  Gio.simpleActionSetEnabled it choice.usable
  Gio.actionMapAddAction group it
  pure (choice.name, it)

-- | Read the menu and put it behind the button.
--
-- A menu that cannot be read leaves the button with nothing to show,
-- and says so through GLib rather than by throwing. This runs while
-- the window is being built, where there is nobody to catch anything,
-- and a program that says what is missing is better than one that
-- disappears. The file is looked for once at startup as well, which is
-- where a missing one is meant to be caught.
loadMenu :: Gtk.MenuButton -> IO ()
loadMenu button = do
  path  <- dataFile menuFile
  built <- try (Gtk.builderNewFromFile path)
  case built of
    Left failure -> complain
      (  "Could not read the menu from "
      <> Text.pack path
      <> ": "
      <> Text.pack (show (failure :: SomeException))
      )
    Right builder -> do
      found <- Gtk.builderGetObject builder "main"
      model <- maybe (pure Nothing) (Gtk.castTo Gio.MenuModel) found
      case model of
        Nothing -> complain ("There is no menu called main in " <> Text.pack path)
        Just it -> Gtk.menuButtonSetMenuModel button (Just it)

-- | Say something went wrong, where a program that is running says it.
complain :: Text.Text -> IO ()
complain what = GLib.logDefaultHandler (Just "stones")
                                       [GLib.LogLevelFlagsLevelWarning]
                                       (Just what)
                                       nullPtr
