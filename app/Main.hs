-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE OverloadedStrings #-}

-- | Starting the program.
--
-- The window belongs to an @AdwApplication@ rather than being made on
-- its own, because a libadwaita program needs one: the application is
-- what calls @adw_init@, what carries the identifier the desktop knows
-- the program by, and what the window is registered with.
module Main
  ( main
  )
where

import           Control.Monad                  ( void
                                                , when
                                                )
import           Data.Foldable                  ( for_ )
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as Text
import           Options.Applicative
import           System.Directory               ( doesDirectoryExist )
import           System.Environment             ( lookupEnv )
import           System.Exit                    ( exitFailure )
import           System.FilePath                ( (</>) )

import qualified GI.Adw                        as Adw
import qualified GI.Gdk                        as Gdk
import qualified GI.Gio                        as Gio
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative.App.Simple  ( startInApplication )

import           Go.Types
import qualified Stones.App                    as Stones
import qualified Stones.Engine.GnuGo           as GnuGo

-- | What the command line asked for.
data Options = Options
  { optionSize    :: Int
  , optionColor   :: Color
  , optionLevel   :: GnuGo.Level
  , optionProgram :: FilePath
  }

-- | The command line, as a parser of it.
options :: Parser Options
options =
  Options
    <$> option
          (within 2 19 "a board width")
          (  long "size"
          <> metavar "N"
          <> value 19
          <> showDefault
          <> help "Board width, from 2 to 19"
          )
    <*> (   flag'
            White
            (long "white" <> help "Play White, so the engine opens the game")
        <|> flag' Black (long "black" <> help "Play Black, which moves first")
        <|> pure Black
        )
    <*> option
          (GnuGo.Level <$> within (fst GnuGo.levelRange)
                                  (snd GnuGo.levelRange)
                                  "a level")
          (  long "level"
          <> metavar "N"
          <> value GnuGo.defaultLevel
          <> showDefaultWith (\(GnuGo.Level n) -> show n)
          <> help "How hard GNU Go thinks, from 1 to 10"
          )
    <*> strOption
          (  long "engine"
          <> metavar "PATH"
          <> value "gnugo"
          <> showDefault
          <> help "The GNU Go program to run"
          )

-- | A number that has to be between two others, and what to say to
-- somebody who gave one that is not.
within :: Int -> Int -> String -> ReadM Int
within low high what = do
  given <- auto
  if given >= low && given <= high
    then pure given
    else readerError
      (  what
      <> " is from "
      <> show low
      <> " to "
      <> show high
      <> ", and "
      <> show given
      <> " is not"
      )

-- | What the command line looks like, all told.
description :: ParserInfo Options
description = info
  (helper <*> options)
  (  fullDesc
  <> header "stones - play Go against GNU Go"
  <> progDesc
       "Opens a board. Click a point to play there. New Game opens \
       \another board in a tab of its own."
  )

main :: IO ()
main = do
  chosen <- execParser description
  -- The window opens its first game a moment after it appears, so an
  -- engine that is not there would show up as a tab that never starts.
  -- Trying one here turns that into a line on the terminal and an exit
  -- code.
  working <- GnuGo.probe (optionProgram chosen) (optionLevel chosen)
  case working of
    Left problem -> Text.putStrLn problem >> exitFailure
    Right () ->
      GnuGo.withGnuGo (optionProgram chosen) (optionLevel chosen)
        $ \opponents -> do
            application <- Adw.applicationNew (Just identifier)
                                              [Gio.ApplicationFlagsDefaultFlags]
            _ <- Gio.onApplicationActivate application $ do
              useOwnIcon
              void $ startInApplication
                application
                (Stones.application opponents
                                    (optionColor chosen)
                                    (optionSize chosen)
                )
            void (Gio.applicationRun application Nothing)

-- | The name the desktop knows this program by, which is the name of
-- its icon and of the file that describes it.
identifier :: Text
identifier = "com.github.enzuru.Stones"

-- | Show the program's own icon rather than whatever a window with no
-- icon gets.
--
-- An installed copy is found by name, because its icon sits in a
-- directory the theme already looks in. A copy being worked on is not,
-- so the directory it was built in is added to the ones the theme
-- looks in. @STONES_DATA_DIR@ says where that is, and @data@ beside
-- the working directory is where it is when the program is run from
-- its own source.
useOwnIcon :: IO ()
useOwnIcon = do
  told    <- lookupEnv "STONES_DATA_DIR"
  display <- Gdk.displayGetDefault
  for_ display $ \display' -> do
    theme <- Gtk.iconThemeGetForDisplay display'
    let icons = maybe "data" id told </> "icons"
    there <- doesDirectoryExist icons
    when there (Gtk.iconThemeAddSearchPath theme icons)
  Gtk.windowSetDefaultIconName identifier
