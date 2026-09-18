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
import qualified GameTest
import qualified GeometryTest
import qualified GtpTest
import qualified VertexTest

main :: IO ()
main = do
  properties <- and <$> mapM
    checkParallel
    [ BoardTest.tests
    , GameTest.tests
    , VertexTest.tests
    , GeometryTest.tests
    , AppTest.tests
    ]
  protocol <- GtpTest.run
  unless (properties && protocol) exitFailure
