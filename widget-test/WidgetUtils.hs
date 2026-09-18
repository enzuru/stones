{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the widget tests need to talk to GTK.
module WidgetUtils where

import           Control.Concurrent
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import qualified GI.Adw                        as Adw
import qualified GI.GLib                       as GLib
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.State

-- | Run an action on the main loop's thread, and wait for it.
runUI :: IO b -> IO b
runUI action = do
  answer <- newEmptyMVar
  _      <- GLib.idleAdd GLib.PRIORITY_DEFAULT $ do
    action >>= putMVar answer
    pure False
  takeMVar answer

-- | Let the main loop run whatever is waiting on it, and come back
-- when it has. A reference to another widget by name is resolved on
-- the next turn of the loop, once the whole tree is built, so a test
-- that reads one has to give the loop that turn.
settle :: IO ()
settle = runUI (pure ())

-- | Apply a patch, whatever kind it turned out to be.
apply :: Patchable widget => SomeState -> widget a -> widget b -> IO SomeState
apply state before after = case patch state before after of
  Keep      -> pure state
  Modify  f -> f
  Replace f -> f

-- | What a patch decided, without doing it.
data Decision
  = Kept
  | Modified
  | Replaced
  deriving (Eq, Show)

decision :: Patchable widget => SomeState -> widget a -> widget b -> Decision
decision state before after = case patch state before after of
  Keep      -> Kept
  Modify  _ -> Modified
  Replace _ -> Replaced

-- | Every widget below this one, in tree order, this one included.
descendants :: Gtk.IsWidget parent => parent -> IO [Gtk.Widget]
descendants root = Gtk.toWidget root >>= go
 where
  go widget' = do
    below <- childrenOf widget'
    rest  <- concat <$> traverse go below
    pure (widget' : rest)

childrenOf :: Gtk.Widget -> IO [Gtk.Widget]
childrenOf widget' = go =<< Gtk.widgetGetFirstChild widget'
 where
  go Nothing     = pure []
  go (Just next) = (next :) <$> (go =<< Gtk.widgetGetNextSibling next)

-- | The widgets below this one that are of a given kind.
descendantsOf
  :: (Gtk.IsWidget parent, Gtk.GObject found)
  => (Gtk.ManagedPtr found -> found)
  -> parent
  -> IO [found]
descendantsOf kind root =
  catMaybes <$> (traverse (Gtk.castTo kind) =<< descendants root)

-- | The title and subtitle of every @AdwWindowTitle@ below this
-- widget, which is where a window says where its game stands.
titlesUnder :: Gtk.IsWidget parent => parent -> IO [(Text, Text)]
titlesUnder root = do
  found <- descendantsOf Adw.WindowTitle root
  traverse (\title -> (,) <$> Gtk.get title #title <*> Gtk.get title #subtitle)
           found

-- | The widget a built markup made.
widgetOf :: SomeState -> IO Gtk.Widget
widgetOf = someStateWidget
