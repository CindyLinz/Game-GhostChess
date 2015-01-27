{-# LANGUAGE BangPatterns, OverloadedStrings, LambdaCase, FlexibleInstances, MultiWayIf #-}
module Game where

import Control.Monad
import Control.Concurrent.MVar
import Control.Monad.Error.Class

import Data.Int
import Data.List
import Data.Aeson
import Data.Text (Text)

data Side
  = SideA -- 輪到A走棋子
  | SideB -- 輪到B走棋子
  | SideOther
  | SideAny
  | SideBegin -- 等待雙方擺初始棋子
  | SideBeginA -- 等待A擺初始棋子(B已擺好)
  | SideBeginB -- 等待A擺初始棋子(A已擺好)
  | SideFinalA -- 遊戲結束, A贏
  | SideFinalB -- 遊戲結束, B贏
  deriving (Eq, Show)
data GoodBad = Good | Bad deriving (Eq, Show)
data Ghost = NoGhost | Ghost Side GoodBad deriving (Eq, Show)
data GameResult = WinA | WinB | Tie

data Game = Game
  { gameBoard :: [[Ghost]] -- 棋盤
  , gameCurr :: Side -- 輪到誰
  , gameLost :: [Ghost] -- 被吃掉的棋
  } deriving Show

-- .  空格
-- A  SideA 的 ghost 善惡未知
-- B  SideB 的 ghost 善惡未知
-- AO SideA 的 ghost 善
-- BO SideB 的 ghost 善
-- AX SideA 的 ghost 惡
-- BX SideB 的 ghost 惡

initGame :: Game
initGame = Game
  { gameCurr = SideBegin
  , gameLost = []
  , gameBoard =
    [ [NoGhost] ++ replicate 4 (Ghost SideA Good) ++ [NoGhost]
    , [NoGhost] ++ replicate 4 (Ghost SideA Bad) ++ [NoGhost]
    , replicate 6 NoGhost
    , replicate 6 NoGhost
    , [NoGhost] ++ replicate 4 (Ghost SideB Bad) ++ [NoGhost]
    , [NoGhost] ++ replicate 4 (Ghost SideB Good) ++ [NoGhost]
    ]
  }

isFinalSide :: Side -> Bool
isFinalSide SideFinalA = True
isFinalSide SideFinalB = True
isFinalSide _ = False

isFinalGame :: Game -> GameResult
isFinalGame (Game { gameBoard=board, gameLost=lost }) =
  let
    rot = (flip .) . flip

    (aAll, bAll) = rot foldl (0, 0) (lost ++ join board) $ \(aAll, bAll) -> \case
      NoGhost -> (aAll, bAll)
      Ghost SideA _ -> (aAll+1, bAll)
      Ghost SideB _ -> (aAll, bAll+1)

    (aLostGood, aLostBad, bLostGood, bLostBad) = rot foldl (0, 0, 0, 0) lost $ \(aGood, aBad, bGood, bBad) -> \case
      NoGhost -> (aGood, aBad, bGood, bBad)
      Ghost SideA Good -> (aGood+1, aBad, bGood, bBad)
      Ghost SideA Bad -> (aGood, aBad+1, bGood, bBad)
      Ghost SideB Good -> (aGood, aBad, bGood+1, bBad)
      Ghost SideB Bad -> (aGood, aBad, bGood, bBad+1)

  in
    if | aLostGood==4 || bLostBad==4 || bAll<8 -> WinB
       | bLostGood==4 || aLostBad==4 || aAll<8 -> WinA
       | otherwise -> Tie

(!<-) :: Int -> (a -> a) -> ([a] -> [a])
(!<-) i f = go i where
  go _ [] = []
  go 0 (a:as) = f a : as
  go i (a:as) = a : go (i-1) as
infixr 9 !<-

posInRange :: Int -> Int -> Bool
posInRange r c = axisInRange r && axisInRange c where
  axisInRange x = 0<=x && x<=5

markMaybeFinalGame :: Game -> Game
markMaybeFinalGame game = case isFinalGame game of
  Tie -> game
  WinA -> game { gameCurr=SideFinalA }
  WinB -> game { gameCurr=SideFinalB }

playerReady :: Side -> Side -> Game -> Either Text Game
playerReady startSide side (game @ Game {gameCurr=curr}) = case curr of
  SideBegin | side==SideA -> Right $ game {gameCurr = SideBeginB}
            | side==SideB -> Right $ game {gameCurr = SideBeginA}
  SideBeginA | side==SideA -> Right $ game {gameCurr = startSide}
  SideBeginB | side==SideB -> Right $ game {gameCurr = startSide}
  _ -> Left "不是輪到你"

moveGhost :: (Int, Int) -> (Int, Int) -> Side -> Game -> Either Text Game
moveGhost (r0,c0) (r1,c1) side (game @ Game {gameCurr=curr, gameBoard=board, gameLost=lost}) = do
  unless (posInRange r0 c0 && posInRange r1 c1) (throwError "位置坐標不正確")
  case curr of
    SideA | side==SideA -> goMove
    SideB | side==SideB -> goMove
    SideBegin | side==SideA || side==SideB -> goSwap
    SideBeginA | side==SideA -> goSwap
    SideBeginB | side==SideB -> goSwap
    _ -> Left "不是輪到你"
  where

  ghost0 = board !! r0 !! c0
  ghost1 = board !! r1 !! c1

  goSwap = do
    forM_ [ghost0, ghost1] $ \case
      Ghost gside _ | gside==side -> return ()
      _ -> throwError "只能交換自己的幽靈"

    let
      board'
        = r1 !<- c1 !<- const ghost0
        $ r0 !<- c0 !<- const ghost1
        $ board

    return $ Game { gameBoard=board', gameCurr=curr, gameLost=lost }

  goMove = do
    unless ((r0==r1 && (c0==c1-1 || c0==c1+1)) || (c0==c1 && (r0==r1-1 || r0==r1+1)))
      (throwError "只能往臨格移動")

    case ghost0 of
      Ghost gside _ | gside==side -> return ()
      _ -> throwError "這裡沒有你的幽靈"

    case ghost1 of
      Ghost gside _ | gside==side -> throwError "不要吃自己的幽靈"
      _ -> return ()

    let
      lost' = case ghost1 of
        NoGhost -> lost
        _ -> ghost1 : lost

      curr' = case curr of
        SideA -> SideB
        SideB -> SideA

      board'
        = r1 !<- c1 !<- const ghost0
        $ r0 !<- c0 !<- const NoGhost
        $ board

    return $ Game { gameBoard=board', gameCurr=curr', gameLost=lost' }

escapeGhost :: Side -> Game -> Either Text Game
escapeGhost side (game @ Game { gameCurr=curr, gameBoard=board }) =
  if curr==side
    then case side of
      SideA | Ghost SideA Good <- board !! 5 !! 0 -> dropGhost 5 0
            | Ghost SideA Good <- board !! 5 !! 5 -> dropGhost 5 5
      SideB | Ghost SideB Good <- board !! 0 !! 0 -> dropGhost 0 0
            | Ghost SideB Good <- board !! 0 !! 5 -> dropGhost 0 5
      _ -> throwError "沒有可以脫離的幽靈"
    else
      throwError "不是輪到你"
  where

  dropGhost r c = return $ game { gameBoard = r !<- c !<- const NoGhost $ board }

ghostName :: Side -> Ghost -> String
ghostName side = \case
  NoGhost -> "."
  Ghost gside ggood -> hd : tl where
    hd = case gside of
      SideA -> 'A'
      SideB -> 'B'
    tl = if side `elem` [gside, SideFinalA, SideFinalB, SideAny]
      then case ggood of
        Good -> "O"
        Bad -> "X"
      else
        ""

instance ToJSON (Side, Game) where
  toJSON (side, (Game { gameBoard=board, gameLost=lost, gameCurr=curr })) =
    object
      [ "curr" .= show curr
      , "board" .= map (map (ghostName (if isFinalSide curr then SideAny else side))) board
      , "lost" .= map (ghostName SideAny) lost
      ]
