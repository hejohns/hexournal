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
  id #unref app
  putStrLn "test"

def = id
