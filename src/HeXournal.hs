-- HeXournal.hs

module HeXournal ( hexournal
                 , defaultConfig
                 ) where

import HeXournal.Config

hexournal :: HeXournalConfig -> IO ()
hexournal conf = putStrLn . show $ conf.z
