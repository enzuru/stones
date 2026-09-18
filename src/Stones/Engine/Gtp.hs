{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Talking to a Go program over the Go Text Protocol.
--
-- The protocol is a line protocol on the program's standard input and
-- output. You send a command on one line. The program answers with a
-- line that starts with @=@ for success or @?@ for failure, and then
-- with as many more lines as the answer needs, and then with a blank
-- line. So a reader knows an answer has ended when it reads a line
-- with nothing on it.
--
-- One lock guards the pair of handles. An application sends commands
-- from whatever thread the work is on, and the protocol has no way to
-- tell two answers apart if two commands are in flight, so the lock is
-- what keeps a question next to its own answer.
module Stones.Engine.Gtp
  ( Gtp
  , GtpError(..)
  , describeGtpError
  , start
  , stop
  , command
  , command_
  , alive
  )
where

import           Control.Concurrent.MVar
import           Control.Exception              ( IOException
                                                , SomeException
                                                , try
                                                )
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           System.IO
import           System.Process

-- | A Go program that is running and listening for commands.
data Gtp = Gtp
  { gtpIn      :: Handle
  , gtpOut     :: Handle
  , gtpProcess :: ProcessHandle
  , gtpLock    :: MVar ()
  , gtpCounter :: IORef Int
  , gtpName    :: Text
  }

-- | What can go wrong with a command.
data GtpError
  = Refused Text
    -- ^ The program answered, and the answer was a refusal. The text
    -- is what it said.
  | Broken Text
    -- ^ The program could not be reached: it exited, or the pipe to it
    -- closed.
  deriving (Eq, Show)

-- | What to tell the player about a failed command.
describeGtpError :: GtpError -> Text
describeGtpError = \case
  Refused message -> "The engine refused the move: " <> message
  Broken  message -> "Lost the engine: " <> message

-- | Start a Go program and speak the protocol to it.
--
-- The program is started with its input and output as pipes and its
-- errors passed through to this program's own, so that a program that
-- complains is heard.
start :: FilePath -> [String] -> IO Gtp
start program arguments = do
  (Just input, Just output, _, handle') <- createProcess (proc program arguments)
    { std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = Inherit
    }
  hSetBuffering input  LineBuffering
  hSetBuffering output LineBuffering
  hSetEncoding  input  utf8
  hSetEncoding  output utf8
  lock    <- newMVar ()
  counter <- newIORef 0
  pure Gtp { gtpIn      = input
           , gtpOut     = output
           , gtpProcess = handle'
           , gtpLock    = lock
           , gtpCounter = counter
           , gtpName    = Text.pack program
           }

-- | Ask the program to quit, and wait for it. A program that will not
-- quit is killed, so that this never hangs on the way out.
stop :: Gtp -> IO ()
stop gtp = do
  _ <- (try (command gtp "quit") :: IO (Either SomeException (Either GtpError Text)))
  _ <- (try (hClose (gtpIn gtp)) :: IO (Either IOException ()))
  terminateProcess (gtpProcess gtp)
  _ <- waitForProcess (gtpProcess gtp)
  pure ()

-- | Is the program still running?
alive :: Gtp -> IO Bool
alive gtp = (== Nothing) <$> getProcessExitCode (gtpProcess gtp)

-- | Send a command and read the answer, with the leading @=@ and the
-- identifier taken off and the surrounding blank lines dropped.
command :: Gtp -> Text -> IO (Either GtpError Text)
command gtp input = withMVar (gtpLock gtp) $ \() -> do
  identifier <- nextIdentifier gtp
  sent       <- try $ do
    Text.hPutStrLn (gtpIn gtp) (Text.pack (show identifier) <> " " <> input)
    hFlush (gtpIn gtp)
  case sent of
    Left  failure -> pure (Left (Broken (Text.pack (show (failure :: IOException)))))
    Right ()      -> readResponse gtp

-- | Send a command whose answer does not matter, and keep only the
-- failure.
command_ :: Gtp -> Text -> IO (Either GtpError ())
command_ gtp input = fmap (() <$) (command gtp input)

-- | The next identifier to put in front of a command. The protocol
-- echoes it back, which is what makes it possible to tell an answer
-- from a line the program printed on its own.
nextIdentifier :: Gtp -> IO Int
nextIdentifier gtp = atomicModifyIORef' (gtpCounter gtp) (\n -> (n + 1, n + 1))

-- | Read lines until the blank one that ends an answer.
readResponse :: Gtp -> IO (Either GtpError Text)
readResponse gtp = do
  first <- try (readLines [])
  pure $ case first of
    Left failure ->
      Left (Broken (Text.pack (show (failure :: IOException))))
    Right [] -> Left (Broken "the engine closed its output")
    Right (status : rest) ->
      let body = Text.strip (Text.unlines (dropIdentifier status : rest))
      in  if Text.isPrefixOf "?" status
            then Left (Refused (if Text.null body then "no reason given" else body))
            else Right body
 where
  -- An answer runs to the first blank line. Lines before the status
  -- line are whatever the program printed on its own, and are dropped.
  readLines seen = do
    ended <- hIsEOF (gtpOut gtp)
    if ended
      then pure (reverse seen)
      else do
        line <- Text.hGetLine (gtpOut gtp)
        let trimmed = Text.stripEnd line
        if Text.null trimmed
          then pure (reverse seen)
          else if null seen && not (isStatus trimmed)
            then readLines seen
            else readLines (trimmed : seen)

  isStatus line = Text.isPrefixOf "=" line || Text.isPrefixOf "?" line

-- | Take the @=@ or @?@ and the echoed identifier off the status line.
dropIdentifier :: Text -> Text
dropIdentifier line =
  Text.stripStart (Text.dropWhile (`elem` ("0123456789" :: String)) (Text.drop 1 line))
