{-# LANGUAGE OverloadedStrings, LambdaCase, NamedFieldPuns #-}
module Room where

import qualified Network.WebSockets as WS

import Control.Applicative
import Control.Monad
import Control.Concurrent.MVar
import Control.Monad.Writer

import Data.Aeson
import Data.Text (Text)
import Data.Traversable hiding (forM)
import Data.Time
import Data.Monoid
import qualified Data.Foldable as F

import Game
import Player

data Room = Room
  { roomGame :: MVar Game
  , roomPa :: MVar (Maybe Player)
  , roomPb :: MVar (Maybe Player)
  , roomWatch :: MVar [Player]
  , roomEmpty :: MVar (Maybe UTCTime)
  }

newRoom :: IO Room
newRoom = Room
  <$> newMVar initGame
  <*> newMVar Nothing
  <*> newMVar Nothing
  <*> newMVar []
  <*> newMVar Nothing

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
  let ident = playerIdent player
  resA <- modifyMVar (roomPa room) $ \case
    Nothing ->
      return (Just (player {playerOnline = 1}), SideA)
    Just a
      | playerIdent a == ident ->
        return (Just (player {playerOnline = playerOnline a + 1}), SideA)
      | otherwise -> do
        bRes <- modifyMVar (roomPb room) $ \case
          Nothing -> return (Just (player {playerOnline = 1}), SideB)
          Just b
            | playerIdent b == ident ->
              return (Just (player {playerOnline = playerOnline b + 1}), SideB)
            | otherwise -> do
              modifyMVar_ (roomWatch room) $ \ws -> do
                let
                  go [] = [player {playerOnline = 1}]
                  go (w:ws) =
                    if playerIdent w == ident
                      then (w {playerOnline = playerOnline w + 1}) : ws
                      else w : go ws
                return $ go ws
              return (Just b, SideOther)
        return (Just a, bRes)
  return resA

leaveRoom :: Player -> Room -> IO Side
leaveRoom player room = do
  let ident = playerIdent player
  resA <- modifyMVar (roomPa room) $ \case
    Just a | playerIdent a == ident ->
      return (Just (a {playerOnline = playerOnline a - 1}), SideA)
    ma -> do
      bRes <- modifyMVar (roomPb room) $ \case
        Just b | playerIdent b == ident ->
          return (Just (b {playerOnline = playerOnline b - 1}), SideB)
        mb -> do
          modifyMVar_ (roomWatch room) $ \ws -> do
            let
              go [] = []
              go (w:ws) =
                if playerIdent w == ident
                  then
                    if playerOnline w == 1
                      then ws
                      else (w {playerOnline = playerOnline w - 1}) : ws
                  else
                    w : go ws
            return $ go ws
          return (mb, SideOther)
      return (ma, bRes)
  return resA

checkRoomOnline :: Room -> IO ()
checkRoomOnline room = do
  count <- execWriterT $ do
    let
      countOnline :: Traversable t => (Room -> MVar (t Player)) -> WriterT (Sum Int) IO ()
      countOnline mvField = do
        v <- lift $ readMVar (mvField room)
        flip traverse v $ flip when (tell 1) . (> 0) . playerOnline
        return ()
    countOnline roomPa
    countOnline roomPb
    countOnline roomWatch

  if count == 0
    then do
      modifyMVar_ (roomEmpty room) $ \case
        Nothing -> do
          fmap Just getCurrentTime
        Just time -> do
          return $ Just time
    else do
      swapMVar (roomEmpty room) Nothing
      return ()

needCloseRoom :: Room -> IO Bool
needCloseRoom (Room {roomEmpty}) = do
  mEmpty <- readMVar roomEmpty
  case mEmpty of
    Nothing ->
      return False
    Just time -> do
      now <- getCurrentTime
      return $ diffUTCTime now time > 600

roomPlayerJSON :: Room -> IO Value
roomPlayerJSON (Room {roomPa, roomPb}) = do
  pa <- readMVar roomPa
  pb <- readMVar roomPb

  return $ toJSON
    [ toJSON pa
    , toJSON pb
    ]
