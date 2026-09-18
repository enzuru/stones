-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

-- | Every test the program has.
--
-- None of these need a display or a window. The rules, the names, and
-- the geometry are pure functions, and the protocol test is a
-- subprocess, so this runs anywhere the compiler does.
module Main
  ( main
  )
where

import           Control.Monad                  ( unless )
import           Hedgehog                       ( checkParallel )
import           System.Exit                    ( exitFailure )

import qualified AppTest
import qualified BoardTest
import qualified DrawTest
import qualified FakeEngineTest
import qualified GameTest
import qualified GeometryTest
import qualified GtpTest
import qualified ProtocolTest
import qualified SessionTest
import qualified TypesTest
import qualified VertexTest

main :: IO ()
main = do
  properties <- and <$> mapM
    checkParallel
    [ TypesTest.tests
    , BoardTest.tests
    , GameTest.tests
    , VertexTest.tests
    , GeometryTest.tests
    , DrawTest.tests
    , ProtocolTest.tests
    , FakeEngineTest.tests
    , SessionTest.tests
    , AppTest.tests
    ]
  protocol <- GtpTest.run
  unless (properties && protocol) exitFailure
