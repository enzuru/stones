{-# LANGUAGE LambdaCase        #-}
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

import           Control.Exception              ( bracket )
import           Control.Monad                  ( void )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           System.Environment             ( getArgs
                                                , getProgName
                                                )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )

import qualified GI.Adw                        as Adw
import qualified GI.Gio                        as Gio
import           GI.Gtk.Declarative.App.Simple  ( startInApplication )

import           Go.Types
import qualified Stones.App                    as Stones
import           Stones.Engine                  ( engineClose )
import qualified Stones.Engine.GnuGo           as GnuGo

-- | What the command line asked for.
data Options = Options
  { optionSize    :: Int
  , optionColor   :: Color
  , optionLevel   :: GnuGo.Level
  , optionProgram :: FilePath
  }

-- | A 19x19 game against GNU Go at its usual strength, playing Black.
defaultOptions :: Options
defaultOptions = Options { optionSize    = 19
                         , optionColor   = Black
                         , optionLevel   = GnuGo.defaultLevel
                         , optionProgram = "gnugo"
                         }

main :: IO ()
main = do
  arguments <- getArgs
  case parseOptions arguments defaultOptions of
    Left  message -> failWith message
    Right Nothing -> usage >>= Text.putStrLn >> exitSuccess
    Right (Just options) -> do
      opened <- GnuGo.open (optionProgram options)
                           (optionLevel options)
                           (optionSize options)
      case opened of
        Left  problem -> failWith problem
        Right engine  -> bracket (pure engine) engineClose $ \engine' -> do
          application <- Adw.applicationNew (Just "com.github.enzuru.Stones")
                                            [Gio.ApplicationFlagsDefaultFlags]
          _ <- Gio.onApplicationActivate application . void $ startInApplication
            application
            (Stones.application engine'
                                (optionColor options)
                                (optionSize options)
            )
          void (Gio.applicationRun application Nothing)

-- | Say what went wrong and stop.
failWith :: Text -> IO a
failWith message = Text.putStrLn message >> exitFailure

-- | Read the command line. An unknown argument is an error rather than
-- something to ignore, so that a misspelled option is not silently
-- dropped.
parseOptions :: [String] -> Options -> Either Text (Maybe Options)
parseOptions []       options = Right (Just options)
parseOptions (a : as) options = case a of
  "--help" -> Right Nothing
  "-h"     -> Right Nothing
  "--black" -> parseOptions as options { optionColor = Black }
  "--white" -> parseOptions as options { optionColor = White }
  "--size"  -> withValue as $ \value rest -> case reads value of
    [(n, "")] | n >= 2 && n <= 19 -> parseOptions rest options { optionSize = n }
    _ -> Left "--size takes a board width from 2 to 19."
  "--level" -> withValue as $ \value rest -> case reads value of
    [(n, "")] | n >= fst GnuGo.levelRange && n <= snd GnuGo.levelRange ->
      parseOptions rest options { optionLevel = GnuGo.Level n }
    _ ->
      Left
        (  "--level takes a strength from "
        <> Text.pack (show (fst GnuGo.levelRange))
        <> " to "
        <> Text.pack (show (snd GnuGo.levelRange))
        <> "."
        )
  "--engine" -> withValue as
    $ \value rest -> parseOptions rest options { optionProgram = value }
  _ -> Left ("Unknown option: " <> Text.pack a)
 where
  withValue (value : rest) continue = continue value rest
  withValue [] _ = Left (Text.pack a <> " needs a value after it.")

-- | What the program accepts.
usage :: IO Text
usage = do
  name <- getProgName
  pure $ Text.unlines
    [ "Stones: play Go against GNU Go."
    , ""
    , "Usage: " <> Text.pack name <> " [options]"
    , ""
    , "  --size <n>       Board width, from 2 to 19. The default is 19."
    , "  --black          Play Black, which moves first. This is the default."
    , "  --white          Play White, so the engine opens."
    , "  --level <n>      How hard GNU Go thinks, from 1 to 10. The default is 10."
    , "  --engine <path>  The GNU Go program to run. The default is gnugo."
    , "  --help           Print this and stop."
    ]
