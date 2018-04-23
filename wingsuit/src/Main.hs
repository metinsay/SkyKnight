{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Base
import Game (Game)
import qualified Game as G
import qualified Level as L
import qualified Menu as M

data State
    = Menu
    | Game Game

makePrisms ''State

main :: IO ()
main = play FullScreen (makeColor 1 1 1 1) 60 Menu render handle step

render :: State -> Picture
render Menu = M.render
render (Game g) = G.render g

handle :: Event -> State -> State
handle (EventKey (Char 'm') Down _ _) _ = Menu
handle e Menu = maybe Menu (Game . G.create) (M.handle e)
handle e (Game g) = Game $ G.handle e g

step :: Float -> State -> State
step t s = s & _Game %~ G.step t
