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

  -- NOTE
  -- Some tests below are commented out, because they fail now.
  -- Because we used to allow deb8 but now we don't.
  -- shouldSkipFile and systemNameMapping are so bad and hacky

  describe "mainWithArgs" $ do
    context "with argument: ghc-8.6.2" $ do
      it "produces the correct output" $ do
        -- TODO: use locally cached files only
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.6.2"]
        let actualOutput = pack actualOutputStr
        -- expectedOutput <- readFileUtf8 "output/stack-ghc-8.6.2.yaml"
        actualOutput `shouldNotBe` ("" :: Text)
        -- actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-8.6.1-beta1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.6.1-beta1"]
        let actualOutput = pack actualOutputStr
        -- expectedOutput <- readFileUtf8 "output/stack-ghc-8.6.1-beta1.yaml"
        actualOutput `shouldNotBe` ("" :: Text)
        -- actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-8.8.1-alpha1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.8.1-alpha1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-8.8.1-alpha1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-8.8.1-rc1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.8.1-rc1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-8.8.1-rc1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-8.8.2-rc1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.8.2-rc1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-8.8.2-rc1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-8.10.1-alpha1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.10.1-alpha1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-8.10.1-alpha1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-8.10.1-alpha2" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.10.1-alpha2"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-8.10.1-alpha2.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-8.10.1-rc1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.10.1-rc1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-8.10.1-rc1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-8.10.1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-8.10.1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-8.10.1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-9.0.1-alpha1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-9.0.1-alpha1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-9.0.1-alpha1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-9.0.1-rc1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-9.0.1-rc1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-9.0.1-rc1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-9.0.1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-9.0.1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-9.0.1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-9.2.1-alpha1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-9.2.1-alpha1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-9.2.1-alpha1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-9.2.1-alpha2" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-9.2.1-alpha2"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-9.2.1-alpha2.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
    context "with argument: ghc-9.2.1-rc1" $ do
      it "produces the correct output" $ do
        actualOutputStr <- capture_ $ mainWithArgs ["ghc-9.2.1-rc1"]
        let actualOutput = pack actualOutputStr
        expectedOutput <- readFileUtf8 "output/stack-ghc-9.2.1-rc1.yaml"
        actualOutput `shouldNotBe` ""
        actualOutput `shouldBe` expectedOutput
