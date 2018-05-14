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
import ImageButton (ImageButton)
import qualified ImageButton as IB
import Image
import Level (Level)
import Levels (levels)
import Scores (Scores, getScore)

data Menu = Menu
    { _location :: Location
    , _buttons :: [(ImageButton, Maybe Location)]
    , _image :: Picture
    , _playImage :: Picture
    , _credits :: Picture
    , _help :: Picture
    }

data Location = Home | Play | Credits | Help

makeLenses ''Menu

create :: IO Menu
create = do
    hb <- homeButtons
    menuPic <- imgToPic 0.75 "assets/menu.png"
    levelPic <- imgToPic 0.75 "assets/level_menu.png"
    creditsPic <- imgToPic 0.75 "assets/credits.png"
    helpPic <- imgToPic 0.75 "assets/help_menu.png"
    pure $ Menu Home hb menuPic levelPic creditsPic helpPic

render :: Scores -> Menu -> Picture
render ss m = case m ^. location of
    Home -> renderHome m
    Play -> renderPlay ss m
    Credits -> renderCredits m
    Help -> renderHelp m

renderHome :: Menu -> Picture
renderHome m = m ^. image <> foldMap IB.render (fst <$> (m ^. buttons))

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
handleHome e m = case filter (IB.handle e . fst) (m ^. buttons) of
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


homeButtons :: IO [(ImageButton, Maybe Location)]
homeButtons = do
     startButton <- IB.create "assets/buttons/start_button.png" (200, 100)
     creditsButton <- IB.create "assets/buttons/credits_button.png" (200, -50)
     quit <- IB.create "assets/buttons/quit_button.png" (200, -200)
     pure $ [ (startButton, Just Play), (creditsButton, Just Credits), (quit, Nothing) ]

playButtons :: Scores -> [(Button, String, Level)]
playButtons ss = zipWith mkLevelButton [-offset .. offset] (M.toList levels)
  where
    offset = fromIntegral (length levels - 1) / 2
    mkLevelButton i (n, l) = case getScore n ss of
        Nothing -> ( Button n (-160, 100 * i - 25) (160, 100 * i + 25), n, l )
        Just s ->
            ( Button
                (n ++ " - " ++ showFFloat (Just 2) s "")
                (-160, 100 * i - 25)
                (160, 100 * i + 25)
            , n
            , l
            )
