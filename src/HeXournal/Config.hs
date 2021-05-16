-- HeXournal/Config.hs

module HeXournal.Config ( HeXournalConfig
                        , defaultConfig
                        ) where

newtype HeXournalConfig = HeXournalConfig { z :: Int
                                          , y :: Int
                                          }

defaultConfig :: HeXournalConfig
defaultConfig = HeXournalConfig { z = 0
                                , y = 1
                                }
