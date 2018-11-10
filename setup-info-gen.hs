{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

-- usage: stack run
-- Modify the hard-coded baseUrl and ghcVersion, ghcDateVersion to taste.
-- Or, for release versions of ghc, this should work:
-- stack run -- ghc-8.6.2

import Data.Semigroup ((<>))

import ClassyPrelude
import qualified Network.HTTP.Simple as HTTP
-- TODO: use this again
-- import qualified System.IO as Sys
import qualified Data.Text as Text
-- TODO: use this again
-- import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import qualified System.Environment as Environment

toUrl :: BaseUrl -> RelativePath -> Url
toUrl (BaseUrl baseUrl) (RelativePath relPathText) = Url $
  baseUrl <> drop (textLength "./") relPathText

data GhcSetupInfo = GhcSetupInfo
  { ghcSetupInfoArch :: Arch
  , ghcSetupInfoVersion :: GhcVersion
  , ghcSetupInfoUrl :: Url
  , ghcSetupInfoContentLength :: ContentLength
  , ghcSetupInfoSha256 :: Sha256Sum
  , ghcSetupInfoSha1 :: Sha1Sum
  } deriving (Eq, Ord, Show)

newtype GhcDateVersion = GhcDateVersion Text
  deriving (Eq, IsString, Ord, Show)
newtype BaseUrl = BaseUrl Text
  deriving (Eq, IsString, Ord, Show)
newtype GhcVersion = GhcVersion Text
  deriving (Eq, IsString, Ord, Show)
newtype Arch = Arch Text
  deriving (Eq, IsString, Ord, Show)
newtype Url = Url Text
  deriving (Eq, IsString, Ord, Show)
newtype Sha256Sum = Sha256Sum Text
  deriving (Eq, IsString, Ord, Show)
newtype Sha1Sum = Sha1Sum Text
  deriving (Eq, IsString, Ord, Show)
newtype RelativePath = RelativePath Text
  deriving (Eq, IsString, Ord, Show)
newtype FileName = FileName Text
  deriving (Eq, IsString, Ord, Show)
newtype SystemName = SystemName Text
  deriving (Eq, IsString, Ord, Show)
newtype ContentLength = ContentLength Int
  deriving (Eq, Ord, Show)

shouldSkipFile :: FileName -> Bool
shouldSkipFile "src" = True
shouldSkipFile "testsuite" = True
shouldSkipFile "windows-extra-src" = True
shouldSkipFile "x86_64-deb8-linux-dwarf" = True -- not sure how to disambiguate from deb8
shouldSkipFile "x86_64-deb9-linux" = True -- also not sure how to disambiguate from deb8
shouldSkipFile _ = False

systemNameMapping :: SystemName -> Maybe Arch
systemNameMapping "i386-deb8-linux" = Just "linux32"
systemNameMapping "i386-unknown-mingw32" = Just "windows32"
systemNameMapping "i386-unknown-mingw32-win10" = Just "windows32"
systemNameMapping "x86_64-apple-darwin" = Just "macosx"
systemNameMapping "x86_64-deb8-linux" = Just "linux64"
systemNameMapping "x86_64-unknown-mingw32" = Just "windows64"
systemNameMapping "x86_64-unknown-mingw32-win10" = Just "windows64"
systemNameMapping "x86_64-fedora27-linux" = Just "linux64-tinfo6"
systemNameMapping "x86_64-unknown-linux" = Just "linux64"
systemNameMapping "x86_64-darwin" = Just "macosx"
systemNameMapping "x86_64-fedora-linux" = Just "linux64-tinfo6"
systemNameMapping "aarch64-deb8-linux" = Just "linux-aarch64"
systemNameMapping "x86_64-portbld-freebsd" = Just "freebsd64"
systemNameMapping "i386-portbld-freebsd" = Just "freebsd32"
systemNameMapping _ = Nothing

urlCorrection :: Url -> Url
urlCorrection "https://downloads.haskell.org/~ghc/8.6.2/ghc-8.6.2-x86_64-darwin.tar.xz"
  = "https://downloads.haskell.org/~ghc/8.6.2/ghc-8.6.2-x86_64-apple-darwin.tar.xz"
urlCorrection a = a

-- TODO: generalize?
stripSurroundings :: GhcDateVersion -> RelativePath -> FileName
stripSurroundings (GhcDateVersion ghcDateVersion) (RelativePath relPath) =
    FileName
  . reverse
  . drop (textLength ".tar.xz")
  . reverse
  . drop (textLength $ "./" <> ghcDateVersion <> "-")
  $ relPath

-- TODO: use this again
-- err :: Text -> IO ()
-- err s = TIO.hPutStrLn Sys.stderr ("***** " <> s)

textLength :: Text -> Int
textLength = length

asSystemName :: FileName -> SystemName
asSystemName (FileName fn) = SystemName fn

data SystemNameParse =
    ShouldSkipFile
  | UnrecognizedFileName
  | FileForArch Arch

parseSystemName :: FileName -> SystemNameParse
parseSystemName fileName
  | shouldSkipFile fileName = ShouldSkipFile
  | otherwise = case systemNameMapping (asSystemName fileName) of
      Just arch -> FileForArch arch
      Nothing -> UnrecognizedFileName

newtype ShaFile = ShaFile String
  deriving (IsString)

loadShas :: ShaFile -> (Text -> shaSum) -> BaseUrl -> IO (Map RelativePath shaSum)
loadShas (ShaFile shaFileStr) mkSha (BaseUrl baseUrl) = do
  req <- HTTP.parseRequest (unpack baseUrl <> "/" <> shaFileStr)
  res <- HTTP.httpBS req
  let textBody = decodeUtf8 $ HTTP.getResponseBody $ res
      bodyLines = Text.lines textBody
  pairs <- mapM lineToShaPair bodyLines
  pure $ Map.fromList pairs
  where
    lineToShaPair line = case Text.words line of
      [shaText, pathText] -> pure (RelativePath pathText, mkSha shaText)
      _ -> fail $ "SHA file line was not in expected format"

loadSha256s :: BaseUrl -> IO (Map RelativePath Sha256Sum)
loadSha256s = loadShas "SHA256SUMS" Sha256Sum

loadSha1s :: BaseUrl -> IO (Map RelativePath Sha1Sum)
loadSha1s = loadShas "SHA1SUMS" Sha1Sum

-- TODO: warn about errors, gracefully degrade rather than fatally crash
loadGhcSetupInfo :: GhcDateVersion -> GhcVersion -> BaseUrl -> IO [GhcSetupInfo]
loadGhcSetupInfo ghcDateVersion ghcVersion baseUrl = do
  sha1s <- loadSha1s baseUrl
  sha256s <- loadSha256s baseUrl
  let parseInfo :: (RelativePath, Sha256Sum) -> IO (Maybe GhcSetupInfo)
      parseInfo (relPath, sha256) = case parseSystemName file of
        ShouldSkipFile -> pure $ Nothing
        FileForArch arch -> do
          sha1 <- case Map.lookup relPath sha1s of
            Just s -> pure s
            Nothing -> fail $ "Missing sha1 for file: " <> relPathStr
          let url = urlCorrection $ toUrl baseUrl relPath
          contentLength <- discoverContentLength url
          pure $ Just $ GhcSetupInfo
            { ghcSetupInfoArch = arch
            , ghcSetupInfoVersion = ghcVersion
            , ghcSetupInfoUrl = url
            , ghcSetupInfoContentLength = contentLength
            , ghcSetupInfoSha256 = sha256
            , ghcSetupInfoSha1 = sha1
            }
        UnrecognizedFileName -> fail $
          "Encountered unrecognized file name: " <> relPathStr
        where
          relPathStr = unpack relPathText
          (RelativePath relPathText) = relPath
          file = stripSurroundings ghcDateVersion relPath
  parseInfoMaybes <- mapM parseInfo $ Map.toAscList sha256s
  pure $ catMaybes parseInfoMaybes

printGhcSetupInfo :: Int -> GhcSetupInfo -> IO ()
printGhcSetupInfo indent info = do
  let Arch arch = ghcSetupInfoArch info
      Url url = ghcSetupInfoUrl info
      GhcVersion ver = ghcSetupInfoVersion info
      ContentLength contentLength = ghcSetupInfoContentLength info
      Sha256Sum sha256 = ghcSetupInfoSha256 info
      Sha1Sum sha1 = ghcSetupInfoSha1 info
      indentText = Text.replicate indent " "
  putStrLn $ indentText <> arch <> ":"
  putStrLn $ indentText <> "    " <> ver <> ":"
  putStrLn $ indentText <> "        url: \"" <> url <> "\""
  putStrLn $ indentText <> "        content-length: " <> tshow contentLength
  putStrLn $ indentText <> "        sha1: " <> sha1
  putStrLn $ indentText <> "        sha256: " <> sha256

printCoda :: GhcDateVersion -> IO ()
printCoda (GhcDateVersion ghcDateVersion) = do
  putStrLn ""
  putStrLn $ "resolver: " <> ghcDateVersion
  putStrLn $ "compiler: " <> ghcDateVersion
  putStrLn "compiler-check: match-exact"
  putStrLn "packages: []"

discoverContentLength :: Url -> IO ContentLength
discoverContentLength (Url url) = do
  req0 <- HTTP.parseRequest (unpack url)
  let req = HTTP.setRequestMethod "HEAD" req0
  res <- HTTP.httpBS req
  -- TODO: ensure a 200 response, otherwise content-length could be wrong
  -- TODO: retries
  contentLengthText <- case map decodeUtf8 (HTTP.getResponseHeader "content-length" res) of
    [r] -> pure r
    [] -> fail $ "Expected to find content-length in headers for url: " <> (unpack url)
    _ -> fail $ "Too many content-length headers for url: " <> (unpack url)
  case readMay contentLengthText of
    Just contentLengthInt -> pure $ ContentLength contentLengthInt
    Nothing -> fail $ "Could not parse to int: " <> unpack contentLengthText

main :: IO ()
main = do
  args <- Environment.getArgs
  -- So far only regular releases of ghc are handled properly with args
  let (ghcDateVersion, ghcVersion, baseUrl) = case args of
        [ghcVerStr]
          | take 4 ghcVerStr == "ghc-" ->
              ( GhcDateVersion $ pack ghcVerStr
              , GhcVersion $ pack $ drop 4 ghcVerStr
              , BaseUrl $ "https://downloads.haskell.org/~ghc/" <> pack (drop 4 ghcVerStr) <> "/"
              )
        -- TODO: above: try to discover GhcDateVersion when given a beta or RC version
        -- TODO: try to discover "latest"
        _ -> ( GhcDateVersion "ghc-8.6.2"
             , GhcVersion "8.6.2"
             , BaseUrl "https://downloads.haskell.org/~ghc/8.6.2/"
             )
  ghcSetupInfos <- loadGhcSetupInfo ghcDateVersion ghcVersion baseUrl
  putStrLn "# This file was generated by a script:"
  putStrLn "# https://github.com/DanBurton/stack-setup-info-gen/blob/master/setup-info-gen.hs"
  putStrLn "setup-info:"
  putStrLn "  ghc:"
  mapM_ (printGhcSetupInfo 4) ghcSetupInfos
  printCoda ghcDateVersion
  pure ()
