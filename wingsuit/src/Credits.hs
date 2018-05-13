{-# LANGUAGE TemplateHaskell #-}
module Credits
    ( create
    , render
    ) where

import Base
import Image

data Credits = Credits
    { _image :: Picture
    }

makeLenses ''Credits

create :: IO Credits
create = Credits <$> imgToPic 1 "assets/credits.png"

render :: Credits -> Picture
render p = p ^. image
