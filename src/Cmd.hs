{-# LANGUAGE OverloadedStrings #-}
module Cmd where

import Control.Applicative

import Data.Aeson
import qualified Data.HashMap.Strict as H

data Cmd
  = CmdReady
  | CmdMove Int Int Int Int
  | CmdEscape
  | CmdUnknown
  deriving Show

instance FromJSON Cmd where
  parseJSON (Object v) =
    case takeStr (H.lookup "cmd" v) of
      Just "ready" -> return CmdReady
      Just "esc" -> return CmdEscape
      Just "move" -> CmdMove
        <$> v .: "r1"
        <*> v .: "c1"
        <*> v .: "r2"
        <*> v .: "c2"
      _ -> return CmdUnknown
    where
      takeStr (Just (String str)) = Just str
      takeStr _ = Nothing
