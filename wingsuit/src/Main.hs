{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Base
import Game (Game)
import qualified Game as G
import Menu (Menu)
import qualified Menu as M
import Scores (Scores, updateScore)

import qualified SDL
import qualified Graphics.UI.SDL.Mixer as Mix
import Foreign
import Control.Concurrent
import Control.Concurrent.STM.TChan (TChan, tryReadTChan, newTChanIO, writeTChan)
import Control.Monad.STM (atomically)

data State = State
    { _sessionId :: Int
    , _scores :: Scores
    , _mode :: Mode
    , _cursor :: Point
    , _musicCh :: TChan FilePath
    , _bgMusicCh :: TChan FilePath
    , _currBg :: String
    }

menuMusic :: String
menuMusic = "assets/sounds2/MenuMusic.wav"

airLoop :: String
airLoop = "assets/sounds2/AirRushLoop.wav"

data Mode
    = Menu Menu
    | Game Game

makeLenses ''State

makePrisms ''Mode

playSound :: TChan FilePath -> TChan FilePath -> IO ()
playSound soundCh backgroundCh = do
    keepAlive soundCh backgroundCh Nothing Nothing

keepAlive :: TChan FilePath -> TChan FilePath -> Maybe Mix.Chunk -> Maybe Mix.Chunk -> IO()
keepAlive soundCh backgroundCh soundToPlay backgroundToPlay = do
    _ <- case soundToPlay of
       Just s -> touchForeignPtr s
       Nothing -> return ()
    case backgroundToPlay of
       Just s -> touchForeignPtr s
       Nothing -> return ()
    threadDelay 50000

    newSound <- atomically $ tryReadTChan soundCh
    case newSound of
       Just p -> do
          newPlay <- Mix.loadWAV p
          _ <- Mix.playChannel 2 newPlay 0
          keepAlive soundCh backgroundCh (Just newPlay) backgroundToPlay
       Nothing -> do
           newBgSound <- atomically $ tryReadTChan backgroundCh
           case newBgSound of
              Just b -> do
                 newPlay <- Mix.loadWAV b
                 _ <- Mix.playChannel 1 newPlay (-1)
                 keepAlive soundCh backgroundCh soundToPlay (Just newPlay)
              Nothing -> keepAlive soundCh backgroundCh soundToPlay backgroundToPlay

main :: IO ()
main = do
    initial <- create
    playIO display (makeColor 0 0 0 1) 60 initial render handle step

display :: Display
display = InWindow "skyknight" (1280, 800) (0, 0)

create :: IO State
create = do
    menu <- M.create
    sId <- randomIO
    bgSound <- newTChanIO
    sound <- newTChanIO
    atomically $ writeTChan bgSound menuMusic
    SDL.initialize [SDL.InitAudio]
    _ <- Mix.openAudio 44100 Mix.AudioS16LSB 2 4096
    _ <- Mix.allocateChannels 3
    _ <- Mix.volume 1 32 -- background volume
    _ <- Mix.volume 2 128 -- sound volume
    _ <- forkOS $ playSound sound bgSound
    pure $ State
        { _sessionId = sId
        , _scores = mempty
        , _mode = Menu menu
        , _cursor = 0
        , _musicCh = sound
        , _bgMusicCh = bgSound
        , _currBg = menuMusic
        }

render :: State -> IO Picture
render s = pure $ case s ^. mode of
    Menu m -> M.render (s ^. scores) m
    Game g -> G.render g

handle :: Event -> State -> IO State
handle (EventMotion p) s = pure $ s & cursor .~ p
handle e s = case s ^. mode of
    Menu m -> case M.handle e (s ^. scores) m of
        Right m' -> do
            atomically $ writeTChan (s ^. musicCh) "assets/sounds2/Menu1.wav"
            --if (s ^. currBg == menuMusic) then return () else
            atomically $ writeTChan (s ^. bgMusicCh) menuMusic
            _ <- pure $ s & currBg .~ menuMusic
            pure $ s & mode .~ Menu m'
        Left Nothing -> exitSuccess
        Left (Just (n, l)) -> do
            atomically $ writeTChan (s ^. musicCh) "assets/sounds2/Menu2.wav"
            --if (s ^. currBg == airLoop) then return () else
            atomically $ writeTChan (s ^. bgMusicCh) airLoop
            _ <- pure $ s & currBg .~ airLoop
            g <- G.create n l
            pure $ s & mode .~ Game g
    Game g -> case G.handle e g of
        Left (n, ms) -> do
            menu <- M.create
            pure $ s & scores %~ maybe id (updateScore n) ms & mode .~ Menu menu
        Right g' -> pure $ s & mode .~ Game g'

step :: Float -> State -> IO State
step t s = (mode . _Game) (G.step t (s ^. cursor) (s ^. sessionId)) s
