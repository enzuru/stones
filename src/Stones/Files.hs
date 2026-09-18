-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Where the files that are not code live.
--
-- The icons and the menu are read from a directory beside the program
-- rather than built into it. @STONES_DATA_DIR@ says where that
-- directory is, and @data@ beside the working directory is where it is
-- when the program is run from its own source.
module Stones.Files
  ( dataDir
  , dataFile
  )
where

import           System.Environment             ( lookupEnv )
import           System.FilePath                ( (</>) )

-- | The directory the program's own files are in.
dataDir :: IO FilePath
dataDir = maybe "data" id <$> lookupEnv "STONES_DATA_DIR"

-- | One file under it.
dataFile :: FilePath -> IO FilePath
dataFile name = (</> name) <$> dataDir
