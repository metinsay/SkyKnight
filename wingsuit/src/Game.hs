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
    , _done :: Maybe Float
    }

makeLenses ''Game

create :: String -> Level -> IO Game
create n l = do
    w <- W.create l
    pure $ Game
        { _name = n
        , _world = w
        , _done = Nothing
        }

render :: Game -> Picture
render g = renderWorld <> renderHud
  where
    renderWorld = uncurry translate (- g ^. world . W.player . P.position) $ W.render (g ^. world)
    renderHud = H.render (g ^. world)

handle :: Event -> Game -> Either (String, Float) Game
handle e g = case g ^. done of
    Nothing -> Right $ g & world %~ W.handle e
    Just s -> case e of
        EventKey _ Down _ _ -> Left (g ^. name, s)
        _ -> Right g

step :: Float -> Game -> Game
step t g = maybe (g & world .~ w' & done .~ d) (const g) (g ^. done)
    where (d, w') = W.step t (g ^. world)
