module Log (log_) where

import Base
import Player (Player)
import qualified Player as P

log_ :: Int -> String -> Player -> Maybe (Maybe Float) -> Float -> Float -> IO ()
log_ sId n p e a t = appendFile "log.txt" . (++ "\n") $ intercalate ","
    [ show sId
    , n
    , show $ p ^. P.position . _1
    , show $ p ^. P.position . _2
    , show $ p ^. P.velocity . _1
    , show $ p ^. P.velocity . _2
    , show $ p ^. P.rotation . _1
    , show $ p ^. P.rotation . _2
    , show a
    , show t
    , maybe "step" (maybe "death" $ ("finish:" ++) . show) e
    ]

--module Log (log_) where
--
--import Base
--import Player (Player)
--import qualified Player as P
--
--data Action = Step | Death | Finish Float
--
--log_ :: String -> World -> Action -> Float -> IO ()
--log_ n p action acorns = appendFile "log.txt" . (++ "\n") $ intercalate ","
--    [ n
--    , show $ p ^. P.position . _1
--    , show $ p ^. P.position . _2
--    , show $ p ^. P.velocity . _1
--    , show $ p ^. P.velocity . _2
--    , show $ p ^. P.rotation . _1
--    , show $ p ^. P.rotation . _2
--    , show acorns
--    , maybe "step" (maybe "death" $ ("finish:" ++) . show) e
--    ]
