{-# LANGUAGE TemplateHaskell #-}

module Audio (Audio, initialize, setBackground, playSound, menuMusic, airLoop) where

import qualified SDL
import qualified SDL.Mixer as Mix
import Control.Concurrent
import Control.Concurrent.STM.TChan (TChan, tryReadTChan, newTChanIO, writeTChan)
import Control.Monad.STM (atomically)
import Data.Default.Class (def)

import Base

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
    _ <- Mix.openAudio def 256
    _ <- forkOS $ playSoundHelper sound bgSound
    pure $ Audio sound bgSound menuMusic


setBackground :: Audio -> String -> IO Audio
setBackground a s = if s == (a ^. curr)
                    then pure a
                    else do
                        atomically $ writeTChan (a ^. bgMusicCh) s
                        pure $ a & curr .~ s

playSound :: Audio -> String -> IO ()
playSound a s = atomically $ writeTChan (a ^. musicCh) s

playSoundHelper :: TChan FilePath -> TChan FilePath -> IO ()
playSoundHelper soundCh backgroundCh = keepAlive soundCh backgroundCh Nothing Nothing

keepAlive :: TChan FilePath -> TChan FilePath -> Maybe Mix.Chunk -> Maybe Mix.Chunk -> IO ()
keepAlive soundCh backgroundCh soundToPlay backgroundToPlay = do
    threadDelay 50000
    newSound <- atomically $ tryReadTChan soundCh
    case newSound of
       Just p -> do
          newPlay <- Mix.load p
          Mix.setVolume 128 newPlay -- sound volume
          _ <- Mix.playOn 2 Mix.Once newPlay
          keepAlive soundCh backgroundCh (Just newPlay) backgroundToPlay
       Nothing -> do
           newBgSound <- atomically $ tryReadTChan backgroundCh
           case newBgSound of
              Just b -> do
                 newPlay <- Mix.load b
                 if b == airLoop
                     then Mix.setVolume 128 newPlay
                     else Mix.setVolume 32 newPlay
                 _ <- Mix.playOn 1 Mix.Forever newPlay
                 keepAlive soundCh backgroundCh soundToPlay (Just newPlay)
              Nothing -> keepAlive soundCh backgroundCh soundToPlay backgroundToPlay
