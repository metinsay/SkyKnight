{-# LANGUAGE TemplateHaskell #-}

module Menu
    ( Menu
    , create
    , handle
    , render
    ) where

import qualified Data.Map as M

import Base
import Button (Button(Button))
import qualified Button as B
import Image
import Level (Level)
import Levels (levels)
import Scores (Scores, getScore)

data Menu = Menu
    { _location :: Location
    , _image :: Picture
    , _playImage :: Picture
    , _credits :: Picture
    , _help :: Picture
    }

data Location = Home | Play | Credits | Help

makeLenses ''Menu

create :: IO Menu
create = Menu Home
    <$> imgToPic 0.75 "assets/menu.png"
    <*> imgToPic 0.75 "assets/level_menu.png"
    <*> imgToPic 0.75 "assets/credits.png"
    <*> imgToPic 0.75 "assets/help_menu.png"

render :: Scores -> Menu -> Picture
render ss m = case m ^. location of
    Home -> renderHome m
    Play -> renderPlay ss m
    Credits -> renderCredits m
    Help -> renderHelp m

renderHome :: Menu -> Picture
renderHome m = m ^. image <> foldMap B.render (fst <$> homeButtons)

renderPlay :: Scores -> Menu -> Picture
renderPlay ss m = m ^. playImage <> foldMap B.render ((^. _1) <$> playButtons ss)

renderCredits :: Menu -> Picture
renderCredits m = m ^. credits

renderHelp :: Menu -> Picture
renderHelp m = m ^. help

handle :: Event -> Scores -> Menu -> Either (Maybe (String, Level)) Menu
handle e ss m = case m ^. location of
    Home -> maybe (Left Nothing) Right (handleHome e m)
    Play -> either (Left . Just) Right (handlePlay e ss m)
    Credits -> Right $ handleCredits e m
    Help -> Right $ handleHelp e m

handleHome :: Event -> Menu -> Maybe Menu
handleHome (EventKey (SpecialKey KeyEsc) Down _ _) _ = Nothing
handleHome e m = case filter (B.handle e . fst) homeButtons of
    [] -> Just m
    ((_, ml) : _) -> maybe Nothing (\l -> Just $ m & location .~ l) ml

handlePlay :: Event -> Scores -> Menu -> Either (String, Level) Menu
handlePlay (EventKey (SpecialKey KeyEsc) Down _ _) _ m = Right $ m & location .~ Home
handlePlay e ss m = go $ playButtons ss
  where
    go [] = case e of
        EventKey (MouseButton LeftButton) Down _ _ -> Right $ m & location .~ Home
        _ -> Right m
    go ((b, n, l) : bs)
        | B.handle e b = Left (n, l)
        | otherwise = go bs

handleCredits :: Event -> Menu -> Menu
handleCredits (EventKey (SpecialKey KeyEsc) Down _ _) m = m & location .~ Home
handleCredits (EventKey (MouseButton LeftButton) Down _ _) m = m & location .~ Home
handleCredits _ m = m

handleHelp :: Event -> Menu -> Menu
handleHelp (EventKey (SpecialKey KeyEsc) Down _ _) m = m & location .~ Home
handleHelp (EventKey (MouseButton LeftButton) Down _ _) m = m & location .~ Home
handleHelp _ m = m

homeButtons :: [(Button, Maybe Location)]
homeButtons =
    [ (Button "Play" (-200, 25) (200, 75), Just Play)
    , (Button "Credits" (-200, -75) (200, -25), Just Credits)
    , (Button "Quit (Esc)" (-200, -175) (200, -125), Nothing)
    ]

playButtons :: Scores -> [(Button, String, Level)]
playButtons ss = zipWith mkLevelButton [-offset .. offset] (M.toList levels)
  where
    mkLevelButton i (n, l) = case getScore n ss of
        Nothing -> ( Button n (-200, 100 * i - 25) (200, 100 * i + 25), n, l )
        Just s -> ( Button (n ++ " - " ++ show s) (-200, 100 * i - 25) (200, 100 * i + 25), n, l )
    offset = fromIntegral (length levels - 1) / 2
