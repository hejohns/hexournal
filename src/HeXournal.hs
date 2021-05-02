{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module HeXournal ( hexournal
                 , def
) where

import HeXournal.UI

import GI.Gtk ()

hexournal :: a -> IO ()
hexournal x = do
  app <- initUI
  putStrLn "called hexournal"

def = id
