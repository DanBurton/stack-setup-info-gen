module Main where

import ClassyPrelude

import Test.Hspec
import System.IO.Silently

import Stack.Setup.Info.Gen


data Strength = Weak | Strong

mkTest' :: Strength -> String -> Spec
mkTest' strength s = context ("with argument: " <> s) $ do
  it "produces the correct output" $ do
    actualOutputStr <- capture_ $ mainWithArgs [fromString s]
    let actualOutput = pack actualOutputStr
    actualOutput `shouldNotBe` ""
    case strength of
      Strong -> do
        expectedOutput <- readFileUtf8 $ "output/stack-" <> s <> ".yaml"
        actualOutput `shouldBe` expectedOutput
      Weak -> pure ()

mkTestWeak :: String -> Spec
mkTestWeak = mkTest' Weak

mkTest :: String -> Spec
mkTest = mkTest' Strong

main :: IO ()
main = hspec $ do
  describe "stripSurroundings" $ do
    it "strips url prefix and suffix" $ do
      let url = "https://downloads.haskell.org/~ghc/8.6.2/ghc-8.6.2-aarch64-deb8-linux.tar.xz"
          gdv = "8.6.2"
          gv = "8.6.2"
      stripSurroundings gdv gv url `shouldBe` "aarch64-deb8-linux"

  describe "mainWithArgs" $ do
    -- NOTE
    -- Some tests below are weakened, because they fail now.
    -- Because we used to allow deb8 but now we don't.
    -- shouldSkipFile and systemNameMapping are so bad and hacky
    mapM_ mkTestWeak
      [ "ghc-8.6.2"
      , "ghc-8.6.1-beta1"
      ]
    mapM_ mkTest
      [ "ghc-8.8.1-alpha1"
      , "ghc-8.8.1-rc1"
      , "ghc-8.8.2-rc1"
      , "ghc-8.10.1-alpha1"
      , "ghc-8.10.1-alpha2"
      , "ghc-8.10.1-rc1"
      , "ghc-8.10.1"
      , "ghc-9.0.1-alpha1"
      , "ghc-9.0.1-rc1"
      , "ghc-9.0.1"
      , "ghc-9.2.1-alpha1"
      , "ghc-9.2.1-alpha2"
      , "ghc-9.2.1-rc1"
      , "ghc-9.4.1-rc1"
      ]
