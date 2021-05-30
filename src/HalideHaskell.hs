{- |
Copyright: (c) 2021 Tom Westerhout
SPDX-License-Identifier: BSD-3-Clause
Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>

See README for more info
-}

module HalideHaskell
       ( someFunc
       ) where


someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)
