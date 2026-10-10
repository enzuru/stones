-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later


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
import qualified Data.List                     as List
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Options.Applicative
import           System.Directory               ( doesDirectoryExist
                                                , doesFileExist
                                                )
import           System.Exit                    ( exitFailure )

import qualified GI.Adw                        as Adw
import qualified GI.Gdk                        as Gdk
import qualified GI.Gio                        as Gio
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative.App.Simple  ( startInApplication )

import           Go.Types
import qualified Stones.App                    as Stones
import           Stones.Files                   ( dataFile )
import qualified Stones.Menu                   as Menu
import           Stones.Setup                   ( Handicapping
                                                , Speed
                                                , Strength(..)
                                                , describeHandicapping
                                                , describeSpeed
                                                , describeStrength
                                                , handicappings
                                                , speeds
                                                )
import           Stones.Setup                   ( Against(..)
                                                , Setup(..)
                                                , defaultSetup
                                                )
import           Stones.Opponent                ( choosing )
import qualified Stones.Opponent.GnuGo         as GnuGo
import qualified Stones.Log                    as Log
import qualified Stones.Online                 as Online

-- | What the command line asked for.
--
-- Everything but the program is what the page a tab opens on starts
-- out asking for. The command line does not start a game any more. It
-- says what the first page should already have chosen, for somebody
-- who plays the same game every time.
data Options = Options
  { opening :: Setup
  , program :: FilePath
  , logTo   :: Maybe FilePath
    -- ^ Where to write down what was said to online-go.com, if
    -- anywhere. Nothing is written unless somebody asked.
  }

-- | The command line, as a parser of it.
options :: Parser Options
options =
  Options
    <$> (   Setup
        <$> (   flag'
                Online
                (  long "online"
                <> help "Open on the page that plays at online-go.com"
                )
            <|> pure defaultSetup.against
            )
        <*> option
              (within 2 19 "a board width")
              (  long "size"
              <> metavar "N"
              <> value defaultSetup.size
              <> showDefault
              <> help "Board width on the first page, from 2 to 19"
              )
        <*> (   flag'
                White
                (long "white" <> help "Play White, so the engine opens the game")
            <|> flag' Black (long "black" <> help "Play Black, which moves first")
            <|> pure defaultSetup.human
            )
        <*> option
              (Strength <$> within 1 10 "a level")
              (  long "strength"
              <> metavar "N"
              <> value defaultSetup.strength
              <> showDefaultWith (Text.unpack . describeStrength)
              <> help "How hard the engine tries, from 1 to 10"
              )
        <*> option
              (stonesDown 0 9 "a handicap")
              (  long "handicap"
              <> metavar "N"
              <> value defaultSetup.handicap
              <> showDefault
              <> help
                   "Stones Black starts with down, 0 or 2 to 9. A small \
                   \board takes fewer, and the engine says so if it will \
                   \not place them."
              )
        <*> option
              speedNamed
              (  long "speed"
              <> metavar "NAME"
              <> value defaultSetup.speed
              <> showDefaultWith (Text.unpack . describeSpeed)
              <> help ("How long a game at online-go.com may take: " <> paces)
              )
        <*> option
              handicapping
              (  long "online-handicap"
              <> metavar "NAME"
              <> value defaultSetup.handicapping
              <> showDefaultWith (Text.unpack . describeHandicapping)
              <> help
                   ("What to ask online-go.com about handicaps, which is \
                    \a preference rather than the number --handicap \
                    \gives an engine: " <> asked)
              )
        <*> switch
              (  long "ranked"
              <> help
                   "Play games at online-go.com for rank. Nothing counts \
                   \towards one unless this is asked for."
              )
        <*> strOption
              (  long "versus"
              <> metavar "NAME"
              <> value ""
              <> help
                   "Who to ask for a game at online-go.com, by name. \
                   \Nothing means whoever else is looking for the same \
                   \game, which is how a person ends up on the other \
                   \side. Naming one is how a program is played against \
                   \on purpose."
              )
        )
    <*> strOption
          (  long "engine"
          <> metavar "PATH"
          <> value "gnugo"
          <> showDefault
          <> help "The GNU Go program to run"
          )
    <*> optional
          (strOption
            (  long "log"
            <> metavar "PATH"
            <> help
                 "Write down everything said to online-go.com, with the \
                 \tokens taken out. For finding out what the server \
                 \actually says."
            )
          )
  where
  paces = List.intercalate ", " (map (Text.unpack . describeSpeed) speeds)
  asked =
    List.intercalate ", " (map (Text.unpack . describeHandicapping) handicappings)

-- | A handicap preference by name, in any case.
handicapping :: ReadM Handicapping
handicapping = do
  given <- str
  case [ how | how <- handicappings, matches how given ] of
    how : _ -> pure how
    []      -> readerError ("there is no handicap setting called " <> given)
 where
  matches how given =
    lower (describeHandicapping how) == lower (Text.pack given)

-- | A speed by name, in any case, because a command line is typed.
speedNamed :: ReadM Speed
speedNamed = do
  given <- str
  case [ how | how <- speeds, matches how given ] of
    how : _ -> pure how
    []      -> readerError ("there is no speed called " <> given)
  where matches how given = lower (describeSpeed how) == lower (Text.pack given)

-- | A word with nothing to tell apart but its letters.
lower :: Text -> Text
lower = Text.toLower

-- | A handicap, which is none or at least two. One stone is not a
-- handicap, it is a move.
stonesDown :: Int -> Int -> String -> ReadM Int
stonesDown low high what = do
  given <- within low high what
  if given == 1
    then readerError "a handicap of one stone is a move, not a handicap"
    else pure given

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
  <> header "stones - play Go"
  <> progDesc
       "Opens a page that asks what to play, on this computer against \
       \GNU Go or at online-go.com against whoever is looking for the \
       \same game. Click a point on the board to play there. New Game \
       \opens another page in a tab of its own."
  )

main :: IO ()
main = do
  chosen <- execParser description
  -- Nothing starts an engine until somebody presses the button on the
  -- page a tab opens on, so an engine that is not there would show up
  -- as a game that never starts, a long way from here. Trying one now
  -- turns that into a line on the terminal and an exit code.
  -- Where everything said to an engine and to online-go.com is
  -- written down, when the command line asked for that.
  log'    <- maybe (pure Log.quiet) Log.toFile chosen.logTo
  working <- GnuGo.probe log' chosen.program chosen.opening
  -- The menu is markup in a file beside the program, and a window
  -- whose menu is missing is a window with no way to start a game.
  menu    <- dataFile Menu.menuFile
  present <- doesFileExist menu
  case working of
    _ | not present ->
      Text.putStrLn ("There is no menu at " <> Text.pack menu <> ".")
        >> exitFailure
    Left problem -> Text.putStrLn problem >> exitFailure
    Right () ->
      -- Both places a game can come from, opened for as long as the
      -- window is. Neither starts anything until somebody asks: no
      -- engine runs and nothing is signed in to.
      GnuGo.withGnuGo log' chosen.program $ \computer ->
        Online.withOnline log' Online.browsing $ \online -> do
          let opponents = choosing computer online
          application <- Adw.applicationNew (Just identifier)
                                            [Gio.ApplicationFlagsDefaultFlags]
          _ <- Gio.onApplicationActivate application $ do
            useOwnIcon
            void $ startInApplication
              application
              (Stones.application opponents chosen.opening)
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
  icons   <- dataFile "icons"
  display <- Gdk.displayGetDefault
  for_ display $ \display' -> do
    theme <- Gtk.iconThemeGetForDisplay display'
    there <- doesDirectoryExist icons
    when there (Gtk.iconThemeAddSearchPath theme icons)
  Gtk.windowSetDefaultIconName identifier
