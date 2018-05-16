{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import qualified Data.ByteString.Lazy as B

import Base
import Game (Game)
import qualified Game as G
import Menu (Menu)
import qualified Menu as M
import Scores (Scores, updateScore)
import qualified Audio as A

data State = State
    { _sessionId :: Int
    , _scores :: Scores
    , _mode :: Mode
    , _cursor :: Point
    , _audio :: A.Audio
    }

data Mode
    = Menu Menu
    | Game Game

makeLenses ''State

makePrisms ''Mode


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
    B.appendFile "scores.json" ""
    ss <- fold . decode <$> B.readFile "scores.json"
    a <- A.initialize
    pure $ State
        { _sessionId = sId
        , _scores = ss
        , _mode = Menu menu
        , _cursor = 0
        , _audio = a
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
            A.playSound (s ^. audio) "assets/sounds2/Menu1.wav"
            a <- A.setBackground (s ^. audio) A.menuMusic
            pure $ s & mode .~ Menu m' & audio .~ a
        Left Nothing -> do
            B.writeFile "scores.json" . encode $ s ^. scores
            exitSuccess
        Left (Just (n, l)) -> do
            A.playSound (s ^. audio) "assets/sounds2/Menu2.wav"
            a <- A.setBackground (s ^. audio) A.airLoop
            g <- G.create n l
            pure $ s & mode .~ Game g & audio .~ a
    Game g -> case G.handle e g of
        Left (n, ms) -> do
            menu <- M.create
            pure $ s & scores %~ maybe id (updateScore n) ms & mode .~ Menu menu
        Right g' -> pure $ s & mode .~ Game g'

step :: Float -> State -> IO State
step t s = do
    case s ^. mode of
        Game g -> do
            g' <- G.step t (s ^. cursor) (s ^. sessionId) g
            case (g' ^. G.sound) of
                Just sound -> A.playSound (s ^. audio) sound
                Nothing -> return ()
            pure $ s & mode .~ Game g'
        Menu _ -> pure s
