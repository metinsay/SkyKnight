{-# LANGUAGE TemplateHaskell #-}

module Game
    ( Game
    , create
    , handle
    , render
    , step
    , sound
    ) where

import Base
import qualified Block as B
import Camera (Camera(Camera))
import qualified Camera as C
import qualified FinishHud as FH
import Hud (Hud)
import qualified Hud as H
import Level (Level)
import Log
import qualified Parallax as Px
import PauseMenu (PauseAction(Play, Quit, Reset))
import qualified PauseMenu as PM
import qualified Player as P
import World (World)
import qualified World as W

data Game = Game
    { _name :: String
    , _parallax :: [Picture]
    , _pause :: PM.PauseMenu
    , _world :: World
    , _hud :: Hud
    , _status :: Status
    , _sound :: Maybe String
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
    px <- Px.create
    h <- H.create
    pm <- PM.create
    pure $ Game
        { _name = n
        , _world = w
        , _parallax = px
        , _pause = pm
        , _status = Start
        , _hud = h
        , _sound = Nothing
        }

render :: Game -> Picture
render g = Px.render camera (g ^. parallax)
        <> W.render camera (g ^. world)
        <> H.render (g ^. world) (g ^. hud)
        <> case g ^. status of
            Paused -> PM.render (g ^. pause)
            Finished s -> FH.render s (g ^. world)
            _ -> mempty
  where
    camera = case g ^. status of
        Start -> levelCamera
        Zooming x -> C.combine x playerCamera levelCamera
        _ -> playerCamera
    playerCamera = uncurry Camera (g ^. world ^. W.player . P.position) camZ
    (_, _, camZ) = g ^. world ^. W.getScaleXY $ (playerX, playerY)
    (playerX, playerY) = g ^. world ^. W.player . P.position
    levelCamera = uncurry Camera levelCenter levelSize
    levelCenter = - 0.5 .* (g ^. world . W.start + B.center (g ^. world . W.finish))
    levelSize = mag (B.center (g ^. world . W.finish) - g ^. world . W.start) / 1300

handle :: Event -> Game -> Either (String, Maybe Float) Game
handle e g
    | Paused <- g ^. status, Just a <- PM.handle e (g ^. pause) = case a of
        Play -> Right $ g & status .~ Playing
        Quit -> Left (g ^. name, Nothing)
        Reset -> Right $ g & status .~ Start & world %~ W.reset
handle (EventKey (Char 'p') Down _ _) g = handlePause g
handle (EventKey (Char 'q') Down _ _) g = case g ^. status of
    Paused -> Left (g ^. name, Nothing)
    _ -> Right g
handle (EventKey (Char 'r') Down _ _) g = case g ^. status of
    Finished _ -> Right g
    _ -> Right $ g & status .~ Start & world %~ W.reset
handle (EventKey (SpecialKey KeyEsc) Down _ _) g = case g ^. status of
    Start -> Left (g ^. name, Nothing)
    Zooming _ -> Left (g ^. name, Nothing)
    _ -> handlePause g
handle (EventKey (MouseButton _) Down _ _) g = handleClick g
handle _ g = Right g

handlePause :: Game -> Either (String, Maybe Float) Game
handlePause g = case g ^. status of
    Playing -> Right $ g & status .~ Paused
    _ -> handleClick g

handleClick :: Game -> Either (String, Maybe Float) Game
handleClick g = case g ^. status of
    Start -> Right $ g & status .~ Zooming 1
    Zooming _ -> Right $ g & status .~ Playing
    Playing -> Right g
    Paused -> Right $ g & status .~ Playing
    Finished s -> Left (g ^. name, Just s)

step :: Float -> Point -> Int -> Game -> IO Game
step t c sId g = case g ^. status of
    Playing -> do
        let (e, snd', w') = W.step t c (g ^. world)
        log_ sId (g ^. name) (g ^. world . W.player) e (W.acornCount $ g ^. world) (g ^. world ^. W.startTime - g ^. world ^. W.time)
        pure $ case e of
            Nothing -> g & world .~ w' & sound .~ snd'
            Just Nothing -> g & world .~ w' & world %~ W.reset & status .~ Start & sound .~ snd'
            Just (Just s) -> g & world .~ w' & status .~ Finished s & sound .~ snd'
    Zooming x -> pure $ g & status .~ bool Playing (Zooming $ x - 0.4 * (1 + x) * t) (x > 0) & sound .~ Nothing
    _ -> pure $ g & sound .~ Nothing
