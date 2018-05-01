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
    { _name :: String
    , _world :: World
    , _status :: Status
    }

data Status
    = Start
    | Zooming Float
    | Playing
    | Paused
    | Finished Float

makeLenses ''Game

create :: String -> Level -> IO Game
create n l = do
    w <- W.create l
    pure $ Game
        { _name = n
        , _world = w
        , _status = Start
        }

render :: Game -> Picture
render g = renderWorld <> renderHud
  where
    renderWorld = case g ^. status of
        Start -> join scale (1200 / viewSize) . uncurry translate viewTrans $ W.render (g ^. world)
        Zooming x -> join scale (1200 / (x * viewSize + (1 - x) * 1200))
                   . uncurry translate (x .* viewTrans + (1 - x) .* playTrans)
                   $ W.render (g ^. world)
        _ -> uncurry translate playTrans $ W.render (g ^. world)
    renderHud = H.render (g ^. world)
    viewSize = mag (B.center (g ^. world . W.finish) - g ^. world . W.start)
    viewTrans = - 0.5 .* (g ^. world . W.start + B.center (g ^. world . W.finish))
    playTrans = - g ^. world . W.player . P.position

handle :: Event -> Game -> Either (String, Float) Game
handle (EventKey (Char 'p') Down _ _) g = case g ^. status of
    Playing -> Right $ g & status .~ Paused
    _ -> handleClick g
handle (EventKey (MouseButton _) Down _ _) g = handleClick g
handle e g = case g ^. status of
    Playing -> Right $ g & world %~ W.handle e
    _ -> Right g

handleClick :: Game -> Either (String, Float) Game
handleClick g = case g ^. status of
    Start -> Right $ g & status .~ Zooming 1
    Zooming _ -> Right g
    Playing -> Right g
    Paused -> Right $ g & status .~ Playing
    Finished s -> Left (g ^. name, s)

step :: Float -> Point -> Game -> Game
step t c g = case g ^. status of
    Playing -> g & world .~ w' & status .~ maybe (g ^. status) Finished s
    Zooming x -> g & status .~ bool Paused (Zooming $ x - 0.4 * (1 + x) * t) (x > 0)
    _ -> g
  where
    (s, w') = W.step t c (g ^. world)
