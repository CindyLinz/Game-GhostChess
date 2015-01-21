{-# LANGUAGE TypeFamilies, TemplateHaskell, MultiParamTypeClasses, OverloadedStrings, NamedFieldPuns #-}
module App where

import qualified Network.WebSockets as WS
import qualified Network.Socket as S

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashTable.IO as H
import qualified Data.Map as M

import Room
import Player
import Game

import Cmd
import WebSocketsData

type HashTable k v = H.CuckooHashTable k v

data GhostChessApp = GhostChessApp
--  { appRooms :: MVar (HashTable Text Room)
--  }
  { appRoom :: Room
  }

--initApp = GhostChessApp <$> (H.new >>= newMVar)
initApp = GhostChessApp <$> newRoom

socketApp :: GhostChessApp -> S.SockAddr -> WS.PendingConnection -> IO ()
socketApp (GhostChessApp {appRoom}) addr pConn = do
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
      }

  side <- joinRoom player appRoom
  clientLog $ "side=" ++ show side
  WS.sendTextData conn $ show side

  broadcastRoomState appRoom

  forever $ do
    cmd <- WS.receiveData conn >>= return . maybe CmdUnknown id . decode
    clientLog $ "got command: " ++ show cmd

    let
      playWith :: (Side -> Game -> Either Text Game) -> IO (Maybe Text)
      playWith act = modifyMVar (roomGame appRoom) $ \game -> do
        case act side game of
          Left msg -> do
            clientLog ("error: " ++ T.unpack msg)
            return (game, Just msg)
          Right game' -> do
            return (markMaybeFinalGame game', Nothing)

    mError <- case cmd of
      CmdReady -> playWith playerReady
      CmdMove r1 c1 r2 c2 -> playWith $ moveGhost (r1, c1) (r2, c2)
      CmdEscape -> playWith escapeGhost
      _ -> return (Just "怪指令")

    case mError of
      Just err -> sendRoomState conn side err appRoom
      Nothing -> broadcastRoomState appRoom

    return ()

--  -- 輸出目前房間
--  roomList <- readMVar appRooms >>= H.toList >>= mapM (\(roomId, room) -> (,) roomId <$> roomSpace room)
--  WS.sendTextData conn (encode $ M.fromList roomList)

--  -- 選房
--  roomid <- WS.receiveData conn :: IO Text

  return ()