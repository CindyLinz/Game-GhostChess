{-# LANGUAGE OverloadedStrings #-}
module Player where

import qualified Network.WebSockets as WS
import qualified Network.Socket as S

import Data.Text (Text)
import Data.Aeson

data Player = Player
  { playerConn :: WS.Connection
  , playerAddr :: S.SockAddr
  , playerIdent :: Text
  , playerNick :: Text
  , playerOnline :: Int
  }

instance ToJSON Player where
  toJSON player = object
    [ "nick" .= playerNick player
    , "addr" .= show (playerAddr player)
    , "online" .= playerOnline player
    ]
