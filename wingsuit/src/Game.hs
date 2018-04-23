{-# LANGUAGE TemplateHaskell #-}

module Game
    ( Game
    , create
    , handle
    , render
    , step
    ) where

import Base
import qualified Block as B
import qualified Hud as H
import Level (Level)
import qualified Player as P
import World (World)
import qualified World as W

data Game = Game
    { _world :: World
    , _done :: Bool
    }

makeLenses ''Game

create :: Level -> Game
create l = Game
    { _world = W.create l
    , _done = False
    }

render :: Game -> Picture
render g = renderWorld <> renderHud
  where
    renderWorld = join scale scaling
        . uncurry translate (- g ^. world . W.player . P.position)
        $ W.render (g ^. world)
    renderHud = H.render (g ^. world)
    scaling = 400 / sqrt (100000 + dist ** 2)
    dist = foldl' min 10000 $
        mag . subtract (g ^. world . W.player . P.position) <$> (B.points =<< g ^. world . W.blocks)

handle :: Event -> Game -> Game
handle e g = bool (g & world %~ W.handle e) g (g ^. done)

step :: Float -> Game -> Game
step t g = bool (g & world .~ w' & done .~ d) g (g ^. done)
    where (d, w') = W.step t (g ^. world)
