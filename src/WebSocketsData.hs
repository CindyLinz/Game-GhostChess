{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module WebSocketsData () where

import Network.WebSockets
import Data.Text as T

instance WebSocketsData String where
  fromLazyByteString = T.unpack . fromLazyByteString
  toLazyByteString = toLazyByteString . T.pack
