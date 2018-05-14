module Log (log_) where

import Base
import Player (Player)
import qualified Player as P

log_ :: Int -> String -> Player -> Maybe (Maybe Float) -> Float -> IO ()
log_ sId n p e a = appendFile "log.txt" . (++ "\n") $ intercalate ","
    [ show sId
    , n
    , show $ p ^. P.position . _1
    , show $ p ^. P.position . _2
    , show $ p ^. P.velocity . _1
    , show $ p ^. P.velocity . _2
    , show $ p ^. P.rotation . _1
    , show $ p ^. P.rotation . _2
    , show a
    , maybe "step" (maybe "death" $ ("finish:" ++) . show) e
    ]
