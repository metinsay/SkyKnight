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
    , _done :: Bool
    }

makeLenses ''Game

create :: String -> Level -> IO Game
create n l = do
    w <- W.create l
    pure $ Game
        { _name = n
        , _world = w
        , _done = False
        }

render :: Game -> Picture
render g = renderWorld <> renderHud
  where
    renderWorld = uncurry translate (- g ^. world . W.player . P.position) $ W.render (g ^. world)
    renderHud = H.render (g ^. world)

handle :: Event -> Game -> Either (String, Float) Game
handle e g = case g ^. done of
    False -> Right $ g & world %~ W.handle e
    True -> case e of
        EventKey _ Down _ _ -> Left (g ^. name, g ^. world . W.time)
        _ -> Right g

step :: Float -> Game -> Game
step t g = bool (g & world .~ w' & done .~ d) g (g ^. done)
    where (d, w') = W.step t (g ^. world)
