{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module HeXournal.Main (
    hexournal
   ,def
) where

import HeXournal.UI

import GI.Gtk ()

hexournal :: a -> IO ()
hexournal x = do
  app <- initUI
  putStrLn "test"

def = id
