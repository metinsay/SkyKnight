{-# LANGUAGE TemplateHaskell #-}

module Audio (Audio, initialize, setBackground, playSound, menuMusic, airLoop) where

import Base
import qualified SDL
import qualified Graphics.UI.SDL.Mixer as Mix
import Foreign
import Control.Concurrent
import Control.Concurrent.STM.TChan (TChan, tryReadTChan, newTChanIO, writeTChan)
import Control.Monad.STM (atomically)

data Audio = Audio
    { _musicCh :: TChan FilePath
    , _bgMusicCh :: TChan FilePath
    , _curr :: String
    }

makeLenses ''Audio

menuMusic :: String
menuMusic = "assets/sounds2/MenuMusic.wav"

airLoop :: String
airLoop = "assets/sounds2/AirRushLoop.wav"

initialize :: IO Audio
initialize = do
    bgSound <- newTChanIO
    sound <- newTChanIO
    atomically $ writeTChan bgSound menuMusic
    SDL.initialize [SDL.InitAudio]
    _ <- Mix.openAudio 44100 Mix.AudioS16LSB 2 4096
    _ <- Mix.allocateChannels 3
    _ <- Mix.volume 1 32 -- background volume
    _ <- Mix.volume 2 128 -- sound volume
    _ <- forkOS $ playSoundHelper sound bgSound
    pure $ Audio sound bgSound menuMusic


setBackground :: Audio -> String -> IO Audio
setBackground a s = if s == (a ^. curr)
                    then pure a
                    else do
                        _ <- if s == airLoop
                             then Mix.volume 1 128
                             else Mix.volume 1 32
                        atomically $ writeTChan (a ^. bgMusicCh) s
                        pure $ a & curr .~ s

playSound :: Audio -> String -> IO ()
playSound a s = atomically $ writeTChan (a ^. musicCh) s

playSoundHelper :: TChan FilePath -> TChan FilePath -> IO ()
playSoundHelper soundCh backgroundCh = do
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
