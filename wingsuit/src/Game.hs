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
import qualified Parallax as Px
import World (World)
import qualified World as W

data Game = Game
    { _name :: String
    , _parallax :: [Picture]
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
    px <- Px.load
    pure $ Game
        { _name = n
        , _world = w
        , _parallax = px
        , _status = Start
        }

render :: Game -> Picture
render g = renderParallax <> renderWorld <> renderHud
  where
    renderParallax = case g ^. status of
        Start -> Px.render (viewTrans) ((1200.0 / viewSize) ** 0.1) (g ^. parallax)
        Zooming x -> Px.render
            (x .* viewTrans + (1 - x) .* playTrans)
            ((1200.0 / (x * viewSize + (1 - x) * 1200)) ** 0.1)
            (g ^. parallax)
        _ -> Px.render (playTrans) 1 (g ^. parallax)
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

handle :: Event -> Game -> Either (String, Maybe Float) Game
handle (EventKey (Char 'q') Down _ _) g = case g ^. status of
    Paused -> Left (g ^. name, Nothing)
    _ -> Right g
handle (EventKey (Char 'p') Down _ _) g = handlePause g
handle (EventKey (SpecialKey KeyEsc) Down _ _) g = handlePause g
handle (EventKey (MouseButton _) Down _ _) g = handleClick g
handle e g = case g ^. status of
    Playing -> Right $ g & world %~ W.handle e
    _ -> Right g

handlePause :: Game -> Either (String, Maybe Float) Game
handlePause g = case g ^. status of
    Playing -> Right $ g & status .~ Paused
    _ -> handleClick g

handleClick :: Game -> Either (String, Maybe Float) Game
handleClick g = case g ^. status of
    Start -> Right $ g & status .~ Zooming 1
    Zooming _ -> Right g
    Playing -> Right g
    Paused -> Right $ g & status .~ Playing
    Finished s -> Left (g ^. name, Just s)

step :: Float -> Point -> Game -> Game
step t c g = case g ^. status of
    Playing -> g & world .~ w' & status .~ maybe (g ^. status) Finished s
    Zooming x -> g & status .~ bool Playing (Zooming $ x - 0.4 * (1 + x) * t) (x > 0)
    _ -> g
  where
    (s, w') = W.step t c (g ^. world)
