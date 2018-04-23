{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Base
import qualified Block as B
import qualified Hud as H
import qualified Level as L
import qualified Image as I
import qualified Player as P
import World (World)
import qualified World as W

data State = State
    { _world :: World
    , _done :: Bool
    } deriving Show

makeLenses ''State

main :: IO ()
main = do
    level4 <- I.loadLevel "levels/level1/art.bmp"
    let levels = [('1', L.level1), ('2', L.level2), ('3', L.level3), ('4', level4)]
    play FullScreen (makeColor 1 1 1 1) 60 initial render (handle(levels)) step

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

type Levels = [(Char, L.Level)]
handle :: Levels -> Event -> State -> State
handle l e s = case e of
            (EventKey (Char k) Down _ _) ->
                case lookup k l of
                    Just level -> s & world .~ W.create level & done .~ False
                    Nothing -> bool (s & world %~ W.handle e) s (s ^. done)
            _ -> bool (s & world %~ W.handle e) s (s ^. done)


step :: Float -> State -> State
step t s = bool (s & world .~ w' & done .~ d) s (s ^. done)
  where
    (d, w') = W.step t (s ^. world)
