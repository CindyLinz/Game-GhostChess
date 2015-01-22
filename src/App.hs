{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings, NamedFieldPuns, LambdaCase, TupleSections #-}
module App where

import qualified Network.WebSockets as WS
import qualified Network.Socket as S

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

import System.Random

import Room
import Player
import Game

import Cmd
import WebSocketsData

data GhostChessApp = GhostChessApp
  { appRooms :: MVar [(Text, Room)]
  }

initApp = do
  app <- GhostChessApp <$> newMVar []
  forkIO $ forever $ do
    threadDelay 10000000
    modifyMVar_ (appRooms app) $ \rooms -> do
      unless (null rooms) $ do
        putStrLn $ "check rooms close: "
        forM_ rooms $ \(roomId, room) -> do
          mEmpty <- readMVar (roomEmpty room)
          putStrLn $ "  " ++ show roomId ++ ": " ++ show mEmpty
      flip filterM rooms $ \(_, room) ->
        fmap not (needCloseRoom room)
  return app

socketApp :: GhostChessApp -> S.SockAddr -> WS.PendingConnection -> IO ()
socketApp (GhostChessApp {appRooms}) addr pConn = do
  conn <- WS.acceptRequest pConn
  WS.forkPingThread conn 30

  let
    clientLog :: String -> IO ()
    clientLog msg = putStrLn $ "socket(" ++ show addr ++ ") " ++ msg

  clientLog "connected"

  -- 設定id
  ident <- WS.receiveData conn
  clientLog $ "ident=" ++ show ident

  -- 設定暱稱
  nick <- WS.receiveData conn
  clientLog $ "nick=" ++ show nick

  let
    player = Player
      { playerConn = conn
      , playerIdent = ident
      , playerNick = nick
      , playerAddr = addr
      , playerOnline = undefined
      }

  -- 輸出目前房間
  roomList <- readMVar appRooms
  roomListJSON <- liftM toJSON $ forM roomList $ \(roomId, room) -> do
    players <- roomPlayerJSON room
    return $ object
      [ "id" .= roomId
      , "player" .= players
      ]
  WS.sendTextData conn (encode roomListJSON)

  -- 選房間
  mRoomEntity <- WS.receiveData conn >>= return . maybe CmdUnknownRoom id . decode >>= \case
    CmdNewRoom -> modifyMVar appRooms $ \rooms -> do
      roomId <- return . T.pack . show =<< randomRIO (100000000, maxBound :: Int)
      room <- newRoom
      return ((roomId, room) : rooms, Just (roomId, room))
    CmdJoinRoom roomId -> modifyMVar appRooms $ \rooms -> do
      return (rooms, (roomId,) <$> lookup roomId rooms)
    CmdUnknownRoom -> do
      return Nothing

  case mRoomEntity of
    Nothing -> return ()
    Just (roomId, room) -> do
      clientLog $ "roomId=" ++ T.unpack roomId
      WS.sendTextData conn roomId

      side <- joinRoom player room
      clientLog $ "side=" ++ show side
      WS.sendTextData conn $ show side

      checkRoomOnline room
      broadcastRoomState room

      let
        whenDisconnect = do
          leaveRoom player room
          checkRoomOnline room

      flip finally whenDisconnect $ do
        forever $ do
          cmd <- WS.receiveData conn >>= return . maybe CmdUnknown id . decode
          clientLog $ "got command: " ++ show cmd

          let
            playWith :: (Side -> Game -> Either Text Game) -> IO (Maybe Text)
            playWith act = modifyMVar (roomGame room) $ \game -> do
              case act side game of
                Left msg -> do
                  clientLog ("error: " ++ T.unpack msg)
                  return (game, Just msg)
                Right game' -> do
                  return (markMaybeFinalGame game', Nothing)

          mError <- case cmd of
            CmdReady -> do
              startSide <- randomRIO (0,1 :: Int) >>= \case
                0 -> return SideA
                1 -> return SideB
              playWith (playerReady startSide)
            CmdMove r1 c1 r2 c2 -> playWith $ moveGhost (r1, c1) (r2, c2)
            CmdEscape -> playWith escapeGhost
            _ -> return (Just "怪指令")

          case mError of
            Just err -> sendRoomState conn side err room
            Nothing -> broadcastRoomState room
