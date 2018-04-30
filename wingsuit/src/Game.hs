{-# LANGUAGE TemplateHaskell #-}

module Game
    ( Game
    , create
    , handle
    , render
    , step
    ) where

import Base
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

data Status = Playing | Paused | Finished Float

makeLenses ''Game

create :: String -> Level -> IO Game
create n l = do
    w <- W.create l
    pure $ Game
        { _name = n
        , _world = w
        , _status = Playing
        }

render :: Game -> Picture
render g = renderWorld <> renderHud
  where
    renderWorld = uncurry translate (- g ^. world . W.player . P.position) $ W.render (g ^. world)
    renderHud = H.render (g ^. world)

handle :: Event -> Game -> Either (String, Float) Game
handle (EventKey (Char 'p') Down _ _) g = Right $ g & status .~ case g ^. status of
    Playing -> Paused
    Paused -> Playing
    Finished s -> Finished s
handle e g = case g ^. status of
    Finished s -> case e of
        EventKey _ Down _ _ -> Left (g ^. name, s)
        _ -> Right g
    _ -> Right $ g & world %~ W.handle e

step :: Float -> Game -> Game
step t g = case g ^. status of
    Playing -> g & world .~ w' & status .~ maybe (g ^. status) Finished s
    _ -> g
  where
    (s, w') = W.step t (g ^. world)
