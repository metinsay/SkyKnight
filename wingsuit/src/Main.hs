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
main = play FullScreen (makeColor 1 1 1 1) 60 initial render handle step

initial :: State
initial = State
    { _scores = mempty
    , _mode = Menu
    }

render :: State -> Picture
render s = case s ^. mode of
    Menu -> M.render (s ^. scores)
    Game g -> G.render g

handle :: Event -> State -> State
handle (EventKey (Char 'm') Down _ _) s = s & mode .~ Menu
handle e s = case s ^. mode of
    Menu -> s & mode .~ maybe Menu (Game . uncurry G.create) (M.handle (s ^. scores) e)
    Game g -> case G.handle e g of
        Left (n, t) -> s & scores %~ updateScore n t & mode .~ Menu
        Right g' -> s & mode .~ Game g'

step :: Float -> State -> State
step t s = s & mode . _Game %~ G.step t
