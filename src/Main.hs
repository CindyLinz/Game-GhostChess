{-# LANGUAGE TypeFamilies, TemplateHaskell #-}
module Main where

import qualified Network.Socket as S
import qualified Network.WebSockets as WS

import Control.Monad
import Control.Exception
import Control.Concurrent

import App

main = do
  app <- initApp
  S.withSocketsDo $ do
    sock <- WS.makeListenSocket "0" 3001
    forever $ do
      (conn, addr) <- S.accept sock
      forkIO $ flip finally (S.sClose conn) $ do
        pending <- WS.makePendingConnection conn WS.defaultConnectionOptions
        socketApp app addr pending
