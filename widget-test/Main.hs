-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

-- | The tests that need GTK.
--
-- Everything that decides anything is tested without a display, in
-- @test/@. These are the ones that build real widgets: that the window
-- is a tree GTK accepts, that a change to the state patches it rather
-- than throwing it away, and that the board widget keeps what it is
-- supposed to keep between patches.
--
-- GTK still refuses to start without a display, so this runs under a
-- nested X server. See the @check-widget@ target.
module Main
  ( main
  )
where

import           Control.Concurrent
import           Control.Monad                  ( unless )
import qualified GI.Adw                        as Adw
import qualified GI.GLib                       as GLib
import           Hedgehog                       ( checkSequential )
import           System.Environment             ( lookupEnv
                                                , setEnv
                                                )
import           System.Exit                    ( exitFailure )
import           System.IO

import qualified GobanWidgetTest
import qualified IconTest
import qualified WindowTest

main :: IO ()
main = do
  -- A nested X server has no GL worth speaking of, and this runs under
  -- one.
  unlessSet "GDK_BACKEND" "x11"
  unlessSet "GSK_RENDERER" "cairo"

  -- adw_init starts GTK as well, and the libadwaita widgets want it:
  -- the style manager the board reads is one of the things it puts in
  -- place.
  Adw.init
  loop   <- GLib.mainLoopNew Nothing False
  passed <- newEmptyMVar
  _      <- forkOS $ do
    -- Sequentially: every one of these hands work to the main loop and
    -- waits for it, and two of them doing that at once would deadlock.
    results <- traverse checkSequential
                        [WindowTest.tests, GobanWidgetTest.tests, IconTest.tests]
    GLib.mainLoopQuit loop
    putMVar passed (and results)
  GLib.mainLoopRun loop
  allPassed <- takeMVar passed
  unless allPassed $ do
    hPutStrLn stderr "Widget tests failed."
    exitFailure

unlessSet :: String -> String -> IO ()
unlessSet name value = do
  already <- lookupEnv name
  case already of
    Just _  -> pure ()
    Nothing -> setEnv name value
