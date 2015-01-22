{-# LANGUAGE OverloadedStrings, LambdaCase, NamedFieldPuns #-}
module Room where

import qualified Network.WebSockets as WS

import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar

import Data.Aeson
import Data.Text (Text)
import Data.Traversable

import Game
import Player

data Room = Room
  { roomGame :: MVar Game
  , roomPa :: MVar (Maybe Player)
  , roomPb :: MVar (Maybe Player)
  , roomWatch :: MVar [Player]
  }

newRoom :: IO Room
newRoom = Room
  <$> newMVar initGame
  <*> newMVar Nothing
  <*> newMVar Nothing
  <*> newMVar []

roomJSON :: Side -> Text -> Room -> IO Value
roomJSON side msg room = do
  game <- readMVar (roomGame room)
  mA <- readMVar (roomPa room)
  mB <- readMVar (roomPb room)
  return $ object
    [ "game" .= toJSON (side, game)
    , "playerA" .= maybe Null toJSON mA
    , "playerB" .= maybe Null toJSON mB
    , "msg" .= msg
    ]

sendRoomState :: WS.Connection -> Side -> Text -> Room -> IO ()
sendRoomState conn side msg room = do
  putStrLn $ "sendRoomState"
  json <- roomJSON side msg room
  WS.sendTextData conn (encode json)

broadcastRoomState :: Room -> IO ()
broadcastRoomState room = do

  let
    send side p = sendRoomState (playerConn p) side "" room

    tr :: Traversable t => Side -> t Player -> IO (t ())
    tr = traverse . send

  tr SideA =<< readMVar (roomPa room)
  tr SideB =<< readMVar (roomPb room)
  tr SideOther =<< readMVar (roomWatch room)

  return ()

joinRoom :: Player -> Room -> IO Side
joinRoom player room = do
  putStrLn "joinRoom begin"
  let ident = playerIdent player
  resA <- modifyMVar (roomPa room) $ \case
    Nothing ->
      return (Just player, SideA)
    Just a
      | playerIdent a == ident ->
        return (Just player, SideA)
      | otherwise -> do
        bRes <- modifyMVar (roomPb room) $ \case
          Nothing -> return (Just player, SideB)
          Just b
            | playerIdent b == ident ->
              return (Just player, SideB)
            | otherwise -> do
              modifyMVar_ (roomWatch room) $ \ws ->
                return $ player : filter (\w -> playerIdent w /= ident) ws
              return (Just b, SideOther)
        return (Just a, bRes)

  putStrLn "joinRoom end"

  return resA

roomPlayerJSON :: Room -> IO Value
roomPlayerJSON (Room {roomPa, roomPb}) = do
  pa <- readMVar roomPa
  pb <- readMVar roomPb

  return $ toJSON
    [ toJSON pa
    , toJSON pb
    ]
