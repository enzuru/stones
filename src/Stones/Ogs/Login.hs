-- SPDX-FileCopyrightText: 2026 Elias Khanzada
-- SPDX-License-Identifier: GPL-3.0-or-later

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Signing in to online-go.com, which happens in a browser.
--
-- This program never sees the password. It sends the person to
-- online-go.com in their own browser, and online-go.com sends the
-- browser back to a socket this program opened on this machine,
-- carrying a code. The code is then traded for a token.
--
-- The trade needs a secret that says the code is being traded by
-- whoever asked for it. A program anybody can unpack has no secret to
-- keep, so instead it makes one up per sign-in, sends the hash of it
-- at the start and the secret itself at the end. That is PKCE, and it
-- is why there is a verifier here.
--
-- Everything but the two calls that touch the network is a function of
-- its arguments, and the browser is an argument too, so the whole of
-- it can be driven without one.
module Stones.Ogs.Login
  ( Tokens(..)
  , Verifier(..)
  , verifierFrom
  , newVerifier
  , challengeFor
  , authorizeFor
  , codeFrom
  , parameters
  , tokensFrom
  , login
  , loginWith
  )
where

import           Control.Exception              ( SomeException
                                                , bracket
                                                , try
                                                )
import           Data.Aeson                     ( FromJSON(..)
                                                , eitherDecodeStrict
                                                , withObject
                                                , (.:)
                                                , (.:?)
                                                )
import           Data.Bifunctor                 ( first )
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Base64.URL    as Base64
import qualified Data.ByteString.Lazy          as Lazy
import           Data.Char                      ( isHexDigit )
import           Data.Foldable                  ( for_ )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Text.Encoding.Error      as Encoding
import qualified Crypto.Hash.SHA256            as Sha256
import qualified Network.HTTP.Client           as Http
import qualified Network.HTTP.Client.TLS       as Http
import           Network.Socket
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )
import           Numeric                        ( readHex )
import           System.Random                  ( randomRIO )

import           Stones.Ogs

-- | What signing in comes back with.
data Tokens = Tokens
  { access  :: Text
  , refresh :: Maybe Text
  , expires :: Maybe Int
    -- ^ Seconds from now.
  }
  deriving (Eq, Show)

instance FromJSON Tokens where
  parseJSON = withObject "tokens" $ \fields ->
    Tokens
      <$> fields
      .:  "access_token"
      <*> fields
      .:? "refresh_token"
      <*> fields
      .:? "expires_in"

-- | The made-up secret of one sign-in.
newtype Verifier = Verifier Text
  deriving (Eq, Show)

-- | The letters a verifier is made of, which the specification calls
-- unreserved: nothing in one needs escaping anywhere it is written.
alphabet :: String
alphabet = ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9'] <> "-._~"

-- | A verifier from text that is already random, for the tests.
--
-- It is refused unless it is long enough and made of the right
-- letters, which is what the specification asks of one.
verifierFrom :: Text -> Either Text Verifier
verifierFrom text
  | Text.length text < 43 = Left "a verifier shorter than 43 letters"
  | Text.length text > 128 = Left "a verifier longer than 128 letters"
  | Text.any (`notElem` alphabet) text = Left "a verifier with an odd letter in it"
  | otherwise = Right (Verifier text)

-- | A fresh one.
newVerifier :: IO Verifier
newVerifier = Verifier . Text.pack <$> traverse pick (replicate 64 ())
 where
  pick _ = (alphabet !!) <$> randomRIO (0, length alphabet - 1)

-- | What is sent at the start: the hash of the verifier, written the
-- way a URL can carry it.
challengeFor :: Verifier -> Text
challengeFor (Verifier text) =
  Text.dropWhileEnd (== '=')
    . Text.decodeUtf8
    . Base64.encode
    . Sha256.hash
    . Text.encodeUtf8
    $ text

-- | Where to send the browser.
authorizeFor :: Text -> Verifier -> Text -> Text
authorizeFor redirect verifier state = authorizeUrl <> "?" <> query
  [ ("response_type"        , "code")
  , ("client_id"            , clientId)
  , ("redirect_uri"         , redirect)
  , ("state"                , state)
  , ("code_challenge"       , challengeFor verifier)
  , ("code_challenge_method", "S256")
  ]

-- | Parameters, joined and escaped.
query :: [(Text, Text)] -> Text
query = Text.intercalate "&" . map one
  where one (name, value) = name <> "=" <> escape value

-- | Escape everything a URL does not carry as itself, a byte at a
-- time, which is what a letter outside these becomes.
escape :: Text -> Text
escape = Text.concatMap one
 where
  one letter
    | letter `elem` alphabet = Text.singleton letter
    | otherwise = Text.concat
      [ "%" <> hex byte
      | byte <- ByteString.unpack (Text.encodeUtf8 (Text.singleton letter))
      ]
  hex byte = Text.pack [digit (byte `div` 16), digit (byte `mod` 16)]
  digit n = "0123456789ABCDEF" !! fromIntegral n

-- | The code the browser brought, out of the request it made.
--
-- The state is checked because it is the only thing that says this
-- request is the answer to the one this program started, rather than
-- something else that found the socket first.
codeFrom :: Text -> Text -> Either Text Text
codeFrom expected request = do
  target <- case Text.words (Text.takeWhile (/= '\r') (firstLine request)) of
    _method : target : _ -> Right target
    _                    -> Left "a request with no target in it"
  let asked = parameters (Text.drop 1 (Text.dropWhile (/= '?') target))
  for_ (lookup "error" asked) $ \why ->
    Left ("online-go.com said no: " <> maybe why id (lookup "error_description" asked))
  case lookup "state" asked of
    Nothing -> Left "an answer with no state in it"
    Just given | given /= expected -> Left "an answer to somebody else's question"
    _ -> pure ()
  case lookup "code" asked of
    Nothing   -> Left "an answer with no code in it"
    Just code -> Right code
  where firstLine = Text.takeWhile (/= '\n')

-- | The name and value pairs of a query string, unescaped.
parameters :: Text -> [(Text, Text)]
parameters text =
  [ (name, unescape (Text.drop 1 rest))
  | piece <- Text.splitOn "&" text
  , not (Text.null piece)
  , let (name, rest) = Text.breakOn "=" piece
  ]

-- | Turn the escapes in a query value back into the letters they
-- stand for. Anything that is not an escape is left as it is.
--
-- An escape stands for one byte and not for one letter, and a letter
-- outside the plain ones is several bytes, so the bytes are collected
-- and read as text at the end rather than one at a time. Reading them
-- one at a time turns one letter into two, which is what a property
-- test found.
--
-- What arrives here came off a socket that anything on this machine
-- can reach, so bytes that are not text are replaced rather than
-- thrown at.
unescape :: Text -> Text
unescape =
  Text.decodeUtf8With Encoding.lenientDecode
    . ByteString.pack
    . go
    . Text.unpack
    . Text.replace "+" " "
 where
  go ('%' : a : b : rest) | isHexDigit a, isHexDigit b =
    case readHex [a, b] of
      [(value, "")] -> fromIntegral (value :: Int) : go rest
      _             -> asBytes '%' <> go (a : b : rest)
  go (letter : rest) = asBytes letter <> go rest
  go []              = []
  asBytes = ByteString.unpack . Text.encodeUtf8 . Text.singleton

-- | Read what the token endpoint answered.
tokensFrom :: ByteString.ByteString -> Either Text Tokens
tokensFrom = first (("online-go.com answered with " <>) . Text.pack)
  . eitherDecodeStrict

-- * Doing it
-------------

-- | Sign in, opening the given action on the address the person has to
-- visit.
login :: (Text -> IO ()) -> IO (Either Text Tokens)
login open = loginWith open exchange

-- | The same, with the trade at the end handed in as well.
--
-- Both of the things that reach outside this machine are arguments:
-- the browser, because this module has no business knowing what one
-- is, and the trade, because it is the only part that talks to
-- online-go.com. What is left is the socket on this machine, and a
-- test drives the whole of it without a network.
loginWith
  :: (Text -> IO ())
  -> (Text -> Verifier -> Text -> IO (Either Text Tokens))
  -> IO (Either Text Tokens)
loginWith open trade = bracket listening (traverse (close . fst)) $ \case
  Left  why               -> pure (Left why)
  Right (listener, port) -> do
    verifier <- newVerifier
    Verifier state' <- newVerifier
    let redirect = redirectFor port
        state    = Text.take 24 state'
    open (authorizeFor redirect verifier state)
    brought <- waiting listener state
    case brought of
      Left  why  -> pure (Left why)
      Right code -> trade redirect verifier code

-- | A socket on the first of the ports that is free.
--
-- More than one because the address the browser comes back to has to
-- be one that was registered, so the port is part of it, and a port
-- can be taken by something else on the machine.
listening :: IO (Either Text (Socket, Int))
listening = go redirectPorts
 where
  go [] = pure
    (Left "every address this program can be sent back to was taken")
  go (port : rest) = try (bind' port) >>= \case
    Left  (_ :: SomeException) -> go rest
    Right listener             -> pure (Right (listener, port))
  bind' port = do
    listener <- socket AF_INET Stream defaultProtocol
    setSocketOption listener ReuseAddr 1
    bindSocket' listener port
    listen listener 1
    pure listener
  bindSocket' listener port =
    bind listener (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))

-- | Wait for the browser, take the code, and leave a page saying so.
waiting :: Socket -> Text -> IO (Either Text Text)
waiting listener expected =
  bracket (accept listener) (close . fst) $ \(conn, _) -> do
    request <- readRequest conn ""
    let brought = codeFrom expected request
    sendAll conn (page brought)
    pure brought

-- | Read until the first line is whole, which is the only line that
-- matters here.
readRequest :: Socket -> Text -> IO Text
readRequest conn sofar
  | Text.isInfixOf "\n" sofar = pure sofar
  | Text.length sofar > 8192 = pure sofar
  | otherwise = do
    more <- recv conn 4096
    if ByteString.null more
      then pure sofar
      -- Lenient for the same reason: this is a socket anything on the
      -- machine can reach, and a request that is not text should be
      -- refused rather than thrown at.
      else readRequest conn (sofar <> Text.decodeUtf8With Encoding.lenientDecode more)

-- | What the browser is left showing.
page :: Either Text Text -> ByteString.ByteString
page brought = Text.encodeUtf8
  (  "HTTP/1.1 200 OK\r\n"
  <> "Content-Type: text/html; charset=utf-8\r\n"
  <> "Connection: close\r\n"
  <> "Content-Length: "
  <> Text.pack (show (ByteString.length body))
  <> "\r\n\r\n"
  )
  <> body
 where
  body = Text.encodeUtf8
    (  "<!doctype html><html><head><meta charset=\"utf-8\">"
    <> "<title>Stones</title></head><body style=\"font-family:sans-serif\">"
    <> "<h1>"
    <> headline
    <> "</h1><p>"
    <> rest
    <> "</p></body></html>"
    )
  (headline, rest) = case brought of
    Right _  -> ("Signed in", "You can close this page and go back to Stones.")
    Left why -> ("Not signed in", why)

-- | Trade the code for a token.
exchange :: Text -> Verifier -> Text -> IO (Either Text Tokens)
exchange redirect (Verifier verifier) code = do
  manager <- Http.newManager Http.tlsManagerSettings
  asked   <- try (Http.parseRequest (Text.unpack ("POST " <> tokenUrl)))
  case asked of
    Left (problem :: SomeException) ->
      pure (Left (Text.pack (show problem)))
    Right blank -> do
      let request = Http.urlEncodedBody
            [ ("grant_type"   , "authorization_code")
            , ("code"         , Text.encodeUtf8 code)
            , ("redirect_uri" , Text.encodeUtf8 redirect)
            , ("client_id"    , Text.encodeUtf8 clientId)
            , ("code_verifier", Text.encodeUtf8 verifier)
            ]
            blank
      answered <- try (Http.httpLbs request manager)
      pure $ case answered of
        Left (problem :: SomeException) ->
          Left ("Could not reach online-go.com: " <> Text.pack (show problem))
        Right answer ->
          tokensFrom (Lazy.toStrict (Http.responseBody answer))
