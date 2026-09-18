-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The program's own icon.
--
-- An icon that is not where the theme looks for it does not fail: the
-- window opens with whatever a window with no icon gets, and nobody
-- notices until somebody looks at the dock. So this asks the theme for
-- it by the name the program uses, and checks that what comes back is
-- the file in this source tree.
module IconTest
  ( tests
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified GI.Gdk                        as Gdk
import qualified GI.Gio                        as Gio
import qualified GI.Gtk                        as Gtk
import           Hedgehog

import           WidgetUtils                    ( runUI )

-- | The name the desktop knows the program by.
identifier :: Text
identifier = "com.github.enzuru.Stones"

-- | The theme, with this source tree's icons among the places it
-- looks. The tests run from the top of the tree, which is where the
-- program looks when it is run from its own source.
themeHere :: IO Gtk.IconTheme
themeHere = do
  display <- Gdk.displayGetDefault
  case display of
    Nothing       -> error "there is no display"
    Just display' -> do
      theme <- Gtk.iconThemeGetForDisplay display'
      Gtk.iconThemeAddSearchPath theme "data/icons"
      pure theme

-- | The file the theme answers with for this name, if it answers with
-- one at all.
fileFor :: Text -> IO (Maybe Text)
fileFor name = do
  theme     <- themeHere
  paintable <- Gtk.iconThemeLookupIcon theme
                                       name
                                       (Nothing :: Maybe [Text])
                                       128
                                       1
                                       Gtk.TextDirectionNone
                                       []
  found <- Gtk.iconPaintableGetFile paintable
  case found of
    Nothing   -> pure Nothing
    Just file -> fmap Text.pack <$> Gio.fileGetPath file

prop_theThemeHasTheProgramsIcon :: Property
prop_theThemeHasTheProgramsIcon = withTests 1 . property $ do
  there <- evalIO (runUI (flip Gtk.iconThemeHasIcon identifier =<< themeHere))
  there === True

prop_theThemeHasTheSymbolicOne :: Property
prop_theThemeHasTheSymbolicOne = withTests 1 . property $ do
  there <- evalIO
    (runUI (flip Gtk.iconThemeHasIcon (identifier <> "-symbolic") =<< themeHere))
  there === True

prop_theIconTheThemeFindsIsTheOneInThisTree :: Property
prop_theIconTheThemeFindsIsTheOneInThisTree = withTests 1 . property $ do
  -- A name that the theme cannot place still answers with a paintable,
  -- for the broken-image icon, so the name is not enough on its own.
  found <- evalIO (runUI (fileFor identifier))
  case found of
    Nothing   -> annotate "the theme gave back no file" >> failure
    Just path -> do
      annotate (Text.unpack path)
      assert (Text.isSuffixOf "/scalable/apps/com.github.enzuru.Stones.svg" path)

prop_theSymbolicIconIsTheOneInThisTree :: Property
prop_theSymbolicIconIsTheOneInThisTree = withTests 1 . property $ do
  found <- evalIO (runUI (fileFor (identifier <> "-symbolic")))
  case found of
    Nothing   -> annotate "the theme gave back no file" >> failure
    Just path -> do
      annotate (Text.unpack path)
      assert
        (Text.isSuffixOf "/symbolic/apps/com.github.enzuru.Stones-symbolic.svg"
                         path
        )

prop_theIconsTheHeaderBarAsksForAreAllThere :: Property
prop_theIconsTheHeaderBarAsksForAreAllThere = withTests 1 . property $ do
  -- These come from the icon theme on the machine rather than from
  -- this tree, and a button whose icon is missing shows a broken image
  -- where the picture should be.
  theme <- evalIO (runUI themeHere)
  let wanted =
        ["edit-undo-symbolic", "media-skip-forward-symbolic", "open-menu-symbolic"]
  found <- evalIO (runUI (traverse (Gtk.iconThemeHasIcon theme) wanted))
  found === map (const True) wanted

tests :: Group
tests = $$(discover)
