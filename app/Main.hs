module Main (main) where

import Stack.Setup.Info.Gen (mainWithArgs)
import qualified System.Environment as Environment
import qualified Data.Text as Text

main :: IO ()
main = Environment.getArgs >>= mainWithArgs . map Text.pack
