module Main where

import ClassyPrelude

import Test.Hspec
import System.IO.Silently

import Stack.Setup.Info.Gen (mainWithArgs)

main :: IO ()
main = hspec $ do
  describe "mainWithArgs" $ do
    context "with argument: ghc-8.6.2" $ do
      it "produces the expected output" $ do
        -- TODO: use locally cached files only
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.6.2"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-8.6.2.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-8.6.1-beta1" $ do
      it "produces the expected output" $ do
        -- TODO: use locally cached files only
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.6.1-beta1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-8.6.1-beta1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
