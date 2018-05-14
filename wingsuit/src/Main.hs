{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Base
import Game (Game)
import qualified Game as G
import Menu (Menu)
import qualified Menu as M
import Scores (Scores, updateScore)

data State = State
    { _sessionId :: Int
    , _scores :: Scores
    , _mode :: Mode
    , _cursor :: Point
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
    pure $ State
        { _sessionId = sId
        , _scores = mempty
        , _mode = Menu menu
        , _cursor = 0
        }

render :: State -> IO Picture
render s = pure $ case s ^. mode of
    Menu m -> M.render (s ^. scores) m
    Game g -> G.render g

handle :: Event -> State -> IO State
handle (EventMotion p) s = pure $ s & cursor .~ p
handle e s = case s ^. mode of
    Menu m -> case M.handle e (s ^. scores) m of
        Right m' -> pure $ s & mode .~ Menu m'
        Left Nothing -> exitSuccess
        Left (Just (n, l)) -> do
            g <- G.create n l
            pure $ s & mode .~ Game g
    Game g -> case G.handle e g of
        Left (n, ms) -> do
            menu <- M.create
            pure $ s & scores %~ maybe id (updateScore n) ms & mode .~ Menu menu
        Right g' -> pure $ s & mode .~ Game g'

step :: Float -> State -> IO State
step t s = (mode . _Game) (G.step t (s ^. cursor) (s ^. sessionId)) s
