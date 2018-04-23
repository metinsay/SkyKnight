{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Base
import Game (Game)
import qualified Game as G
import qualified Menu as M
import Scores (Scores, updateScore)

data State = State
    { _scores :: Scores
    , _mode :: Mode
    }

data Mode
    = Menu
    | Game Game

makeLenses ''State

makePrisms ''Mode

main :: IO ()
main = playIO FullScreen (makeColor 1 1 1 1) 60 initial render handle step

initial :: State
initial = State
    { _scores = mempty
    , _mode = Menu
    }

render :: State -> IO Picture
render s = pure $ case s ^. mode of
    Menu -> M.render (s ^. scores)
    Game g -> G.render g

handle :: Event -> State -> IO State
handle (EventKey (Char 'm') Down _ _) s = pure $ s & mode .~ Menu
handle e s = case s ^. mode of
    Menu -> case M.handle (s ^. scores) e of
        Nothing -> pure s
        Just (n, l) -> do
            g <- G.create n l
            pure $ s & mode .~ Game g
    Game g -> pure $ case G.handle e g of
        Left (n, t) -> s & scores %~ updateScore n t & mode .~ Menu
        Right g' -> s & mode .~ Game g'

step :: Float -> State -> IO State
step t s = pure $ s & mode . _Game %~ G.step t
