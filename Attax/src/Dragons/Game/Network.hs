{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

Module      : Dragons.Game.Network
Description : Send and receive moves over the network
Author      : Dhairya Patel
License     : AllRightsReserved

Networking functionality for the game framework. We treat TCP sockets
as sequences of JSON objects, and fail at the first sniff of an error.
-}
module Dragons.Game.Network
  ( -- * Types
    GameSocket
  , mkGameSocket
    -- * Operations
  , recv
  , send
  ) where

import           Data.Aeson (FromJSON, ToJSON, fromJSON, encode)
import           Data.Aeson.Parser (json)
import           Data.Aeson.Types (Result(..), Value)
import           Data.Attoparsec.ByteString as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Functor (($>))
import           Data.IORef
import           Network.Socket (Socket)
import qualified Network.Socket.ByteString as S

-- | A socket along with any data from a previous read, which we
-- haven't yet tried to parse.
data GameSocket = GameSocket
  { gsSocket :: Socket
  , gsRecvBuffer :: ByteString
  }

-- | Wrap up a 'Socket'. We don't expose the 'GameSocket' constructor
-- to prevent people messing with the buffer.
mkGameSocket :: Socket -> IO (IORef GameSocket)
mkGameSocket s = newIORef $ GameSocket s ""

-- | Blocking read of one record from the socket.
recv :: forall a . FromJSON a => IORef GameSocket -> IO a
recv gsRef = do
  gs <- readIORef gsRef
  let buf = gsRecvBuffer gs
  if B.null buf then loop gs Nothing else decodeResult gs (parse json buf)

  where
    -- Receive chunks from the socket and feed them to the JSON parser.
    loop
      :: GameSocket
      -> Maybe (ByteString -> Atto.Result Value)
      -> IO a
    loop gs mCont = do
      bs <- S.recv (gsSocket gs) 4096
      decodeResult gs $ case mCont of
        Nothing -> parse json bs
        Just cont -> cont bs

    decodeResult :: GameSocket -> Atto.Result Value -> IO a
    decodeResult gs res = case res of
      Fail _ ctx msg -> parseFailure ctx msg
      Partial cont -> loop gs (Just cont)
      Done rest val -> case fromJSON val of
        Error jsonErr -> decodeFailure jsonErr
        Success a -> writeIORef gsRef (gs { gsRecvBuffer = rest }) $> a

-- | Encode a thing and send it out on the socket.
send :: ToJSON a => IORef GameSocket -> a -> IO ()
send gsRef a = readIORef gsRef >>= \gs ->
  S.sendAll (gsSocket gs) . BL.toStrict $ encode a

parseFailure :: [String] -> String -> a
parseFailure context message = error $ concat
  [ "Attoparsec failure: "
  , message
  , "\n. Parser context:\n"
  , unlines context
  ]

decodeFailure :: String -> a
decodeFailure message = error $ "JSON decode failure: " ++ message
