{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.Text.UTF8Proc

main :: IO ()
main = do
  forM_ [minBound .. maxBound] $ \c -> do
    print (c, getProperty $ utf8proc_get_property c)
  print $ breakGraphemes $ utf8proc_NFD "Prajecyrujučy Sinhuliarnaje Wypramieńwańnie Daktryny Absaliutnaha J Usiopahłynaĺnaha Zła Skroź Šaścihrannuju Pryzmu Sîn\x200b\&-\x200b\&Ahhī\x200b\&-\x200b\&Erība Na Hipierp"
