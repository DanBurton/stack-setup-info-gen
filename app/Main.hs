module Main (main) where

import ClassyPrelude
import Stack.Setup.Info.Gen (mainWithArgs)

main :: IO ()
main = getArgs >>= mainWithArgs
