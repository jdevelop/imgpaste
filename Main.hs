module Main where

import ImgPaste.Collect
import System
import Data.List as DL

main = getArgs >>= go
    where
        go [] = putStrLn "Usage: imgpaste dir1 <dir2> <dir3> ..."
        go paths = mapM collectImageTags paths >>= putStrLn . DL.concat
