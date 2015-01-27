{-# LANGUAGE TypeFamilies, TemplateHaskell, LambdaCase #-}
module Main where

import qualified Network.Socket as S
import qualified Network.WebSockets as WS

import Control.Monad
import Control.Exception
import Control.Concurrent

import System.Environment (getArgs)

import App

main = do
  port <- getArgs >>= \case
    [] -> return 3000
    (portStr:_) -> return $ read portStr

  app <- initApp
  S.withSocketsDo $ do
    sock <- WS.makeListenSocket "0" port
    putStrLn $ "websocket server ready on port " ++ show port
    forever $ do
      (conn, addr) <- S.accept sock
      let
        cleanUp = do
          putStrLn $ "socket from " ++ show addr ++ " closed"
          S.sClose conn
        gotException e = do
          putStrLn $ "socket from " ++ show addr ++ " got exception: " ++ show (e :: SomeException)
      forkIO $ flip finally cleanUp $ handle gotException $ do
        pending <- WS.makePendingConnection conn WS.defaultConnectionOptions
        socketApp app addr pending
