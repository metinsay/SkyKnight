{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Base
import qualified Block as B
import qualified Hud as H
import qualified Level as L
import qualified Player as P
import World (World)
import qualified World as W

data State = State
    { _world :: World
    , _done :: Bool
    } deriving Show

makeLenses ''State

main :: IO ()
main = play FullScreen (makeColor 1 1 1 1) 60 initial render handle step

initial :: State
initial = State
    { _world = W.create L.level1
    , _done = False
    }

render :: State -> Picture
render s = renderWorld <> renderHud
  where
    renderWorld = join scale scaling
        . uncurry translate (- s ^. world . W.player . P.position)
        $ W.render (s ^. world)
    renderHud = H.render (s ^. world)
    scaling = 400 / sqrt (100000 + dist ** 2)
    dist = foldl' min 10000 $
        mag . subtract (s ^. world . W.player . P.position) <$> (B.points =<< s ^. world . W.blocks)

handle :: Event -> State -> State
handle (EventKey (Char '1') Down _ _) s = s & world .~ W.create L.level1 & done .~ False
handle (EventKey (Char '2') Down _ _) s = s & world .~ W.create L.level2 & done .~ False
handle (EventKey (Char '3') Down _ _) s = s & world .~ W.create L.level3 & done .~ False
handle e s = bool (s & world %~ W.handle e) s (s ^. done)

step :: Float -> State -> State
step t s = bool (s & world .~ w' & done .~ d) s (s ^. done)
  where
    (d, w') = W.step t (s ^. world)
