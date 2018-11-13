module Main where

import ClassyPrelude

import Test.Hspec
import System.IO.Silently

import Stack.Setup.Info.Gen

main :: IO ()
main = hspec $ do
  describe "stripSurroundings" $ do
    it "strips url prefix and suffix" $ do
      let url = "https://downloads.haskell.org/~ghc/8.6.2/ghc-8.6.2-aarch64-deb8-linux.tar.xz"
          gdv = "8.6.2"
          gv = "8.6.2"
      stripSurroundings gdv gv url `shouldBe` "aarch64-deb8-linux"

  describe "mainWithArgs" $ do
    context "with argument: ghc-8.6.2" $ do
      it "produces the correct output" $ do
        -- TODO: use locally cached files only
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.6.2"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-8.6.2.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-8.6.1-beta1" $ do
      it "produces the correct output" $ do
        -- TODO: use locally cached files only
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.6.1-beta1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-8.6.1-beta1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
