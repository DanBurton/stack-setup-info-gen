{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack.Setup.Info.Gen (mainWithArgs) where

-- usage: stack run
-- Modify the hard-coded baseUrl and ghcVersion, ghcDateVersion to taste.
-- Or, for release versions of ghc, this should work:
-- stack run -- ghc-8.6.2

import Data.Semigroup ((<>))

import ClassyPrelude
import qualified Network.HTTP.Simple as HTTP
-- TODO: use this again
-- import qualified System.IO as Sys
-- TODO: use this again
-- import qualified Data.Text.IO as TIO
import qualified Data.Map as Map

toUrl :: BaseUrl -> RelativePath -> Url
toUrl (BaseUrl baseUrl) (RelativePath relPathText) = Url $
  baseUrl <> drop (length $ asText "./") relPathText

data GhcSetupInfo = GhcSetupInfo
  { ghcSetupInfoArch :: Arch
  , ghcSetupInfoGhcVersion :: GhcVersion
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
stripSurroundings :: GhcVersion -> RelativePath -> FileName
stripSurroundings (GhcVersion ghcVersion) (RelativePath relPath) =
    FileName
  . dropSuffixLength ".tar.xz"
  . dropPrefixLength ("./ghc-" <> ghcVersion <> "-")
  $ relPath

-- TODO: use this again
-- err :: Text -> IO ()
-- err s = TIO.hPutStrLn Sys.stderr ("***** " <> s)

-- like stripPrefix, but without enforcing that it is actually a prefix
dropPrefixLength :: Text -> Text -> Text
dropPrefixLength prefix t = drop (length prefix) t

dropSuffixLength :: Text -> Text -> Text
dropSuffixLength suffix = reverse . dropPrefixLength suffix . reverse

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

newtype ShaFile = ShaFile Text
  deriving (IsString)

loadShas :: ShaFile -> (Text -> shaSum) -> BaseUrl -> IO (Map RelativePath shaSum)
loadShas (ShaFile shaFile) mkSha (BaseUrl baseUrl) = do
  req <- HTTP.parseRequest $ unpack $ baseUrl <> "/" <> shaFile
  res <- HTTP.httpBS req
  let textBody = decodeUtf8 $ HTTP.getResponseBody $ res
      bodyLines = lines textBody
  pairs <- mapM lineToShaPair bodyLines
  pure $ Map.fromList pairs
  where
    lineToShaPair line = case words line of
      [shaText, pathText] -> pure (RelativePath pathText, mkSha shaText)
      _ -> fail $ "SHA file line was not in expected format"

loadSha256s :: BaseUrl -> IO (Map RelativePath Sha256Sum)
loadSha256s = loadShas "SHA256SUMS" Sha256Sum

loadSha1s :: BaseUrl -> IO (Map RelativePath Sha1Sum)
loadSha1s = loadShas "SHA1SUMS" Sha1Sum

-- TODO: warn about errors, gracefully degrade rather than fatally crash
loadGhcSetupInfo :: GhcVersion -> BaseUrl -> IO [GhcSetupInfo]
loadGhcSetupInfo ghcVersion baseUrl = do
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
            , ghcSetupInfoGhcVersion = ghcVersion
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
          file = stripSurroundings ghcVersion relPath
  parseInfoMaybes <- mapM parseInfo $ Map.toAscList sha256s
  pure $ catMaybes parseInfoMaybes

printGhcSetupInfo :: Int -> GhcSetupInfo -> IO ()
printGhcSetupInfo indent info = do
  let Arch arch = ghcSetupInfoArch info
      Url url = ghcSetupInfoUrl info
      GhcVersion ver = ghcSetupInfoGhcVersion info
      ContentLength contentLength = ghcSetupInfoContentLength info
      Sha256Sum sha256 = ghcSetupInfoSha256 info
      Sha1Sum sha1 = ghcSetupInfoSha1 info
      indentText = replicate indent ' ' 
  putStrLn $ indentText <> arch <> ":"
  putStrLn $ indentText <> "    " <> ver <> ":"
  putStrLn $ indentText <> "        url: \"" <> url <> "\""
  putStrLn $ indentText <> "        content-length: " <> tshow contentLength
  putStrLn $ indentText <> "        sha1: " <> sha1
  putStrLn $ indentText <> "        sha256: " <> sha256

printCoda :: GhcVersion -> IO ()
printCoda (GhcVersion ghcVersion) = do
  putStrLn ""
  putStrLn $ "resolver: ghc-" <> ghcVersion
  putStrLn $ "compiler: ghc-" <> ghcVersion
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

baseBaseUrl :: Text
baseBaseUrl = "https://downloads.haskell.org/~ghc/"

-- TODO: better types
-- TODO: implement this by looking at a SHA file
discoverDateVer :: BaseUrl -> IO Text
discoverDateVer "https://downloads.haskell.org/~ghc/8.6.1-beta1/" = pure "8.6.0.20180810"
discoverDateVer (BaseUrl t) = tfail $ "Could not discover ghc version at: " <> t

-- TODO: reduce code duplication
guessGhcVerReps :: Text -> IO (GhcVersion, BaseUrl)
guessGhcVerReps text = case splitElem '-' text of
  [ver] -> pure
    ( GhcVersion ver
    , BaseUrl $ baseBaseUrl <> ver <> "/"
    )
  ["ghc", ver] -> pure
    ( GhcVersion ver
    , BaseUrl $ baseBaseUrl <> ver <> "/"
    )
  ["ghc", prospectiveVer, tag]
    | any (`isPrefixOf` tag) ["alpha", "beta", "rc"] -> do
        let url = BaseUrl $ baseBaseUrl <> prospectiveVer <> "-" <> tag <> "/"
        ver <- discoverDateVer url
        pure
          ( GhcVersion ver
          , url
          )
  [prospectiveVer, tag]
    | any (`isPrefixOf` tag) ["alpha", "beta", "rc"] -> do
        let url = BaseUrl $ baseBaseUrl <> prospectiveVer <> "-" <> tag <> "/"
        ver <- discoverDateVer url
        pure
          ( GhcVersion ver
          , url
          )
  _ -> tfail $ "Could not understand this ghc version: " <> text

tfail :: Text -> IO a
tfail = fail . unpack

mainWithArgs :: [Text] -> IO ()
mainWithArgs args = do
  -- So far only regular releases of ghc are handled properly with args
  verArg <- case args of
    [arg] -> pure arg
    _ -> fail $ "Too many args, expected only 1, got this: " <> show args
  (ghcVersion, baseUrl) <- guessGhcVerReps verArg
  ghcSetupInfos <- loadGhcSetupInfo ghcVersion baseUrl
  putStrLn "# This file was generated by a script:"
  putStrLn "# https://github.com/DanBurton/stack-setup-info-gen/blob/master/setup-info-gen.hs"
  putStrLn "setup-info:"
  putStrLn "  ghc:"
  mapM_ (printGhcSetupInfo 4) ghcSetupInfos
  printCoda ghcVersion
  pure ()
