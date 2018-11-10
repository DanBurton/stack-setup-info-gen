-- usage: stack run
-- modify the baseUrl and ghcVersion, ghcDateVersion to taste
-- Or, for release versions of ghc:
-- stack run -- ghc-8.6.2

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Data.Semigroup ((<>))

import ClassyPrelude
import qualified Data.ByteString.Char8 as C8
import qualified Data.Foldable as F
import qualified Network.HTTP.Simple as HTTP
import qualified System.IO as Sys
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import qualified System.Environment as Environment

-- baseUrl :: Text
-- baseUrl = "https://downloads.haskell.org/~ghc/8.6.2/"

-- ghcDateVersion :: Text
-- ghcDateVersion = "ghc-8.6.2"

-- ghcVersion :: GhcVersion
-- ghcVersion = GhcVersion $ (drop (textLength "ghc-") ghcDateVersion)

toUrl :: BaseUrl -> RelativePath -> Url
toUrl (BaseUrl baseUrl) (RelativePath relPathText) = Url $
  baseUrl <> drop (textLength "./") relPathText

data GhcSetupInfo = GhcSetupInfo
  { ghcSetupInfoArch :: Arch
  , ghcSetupInfoVersion :: GhcVersion
  , ghcSetupInfoUrl :: Url
  , ghcSetupInfoSha256 :: Sha256Sum
  , ghcSetupInfoSha1 :: Sha1Sum
  }
  deriving (Show)

newtype GhcDateVersion = GhcDateVersion { ghcDateVersionText :: Text }
newtype BaseUrl = BaseUrl { baseUrlText :: Text}
  deriving (IsString)
newtype GhcVersion = GhcVersion Text
  deriving (Show)
newtype Arch = Arch { archText :: Text }
  deriving (Eq, IsString, Show)
newtype Url = Url Text
  deriving (Eq, Show, IsString)
newtype Sha256Sum = Sha256Sum Text
  deriving (Show)
newtype Sha1Sum = Sha1Sum Text
  deriving (Show)
newtype RelativePath = RelativePath { relativePathText :: Text }
  deriving (Show, Eq, Ord)
newtype FileName = FileName { fileNameText :: Text }
  deriving (Eq, IsString)
newtype SystemName = SystemName Text
  deriving (Eq, IsString)

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

ghcSetupInfoUrlCorrection :: GhcSetupInfo -> GhcSetupInfo
ghcSetupInfoUrlCorrection info = info
  { ghcSetupInfoUrl = urlCorrection (ghcSetupInfoUrl info) }

-- TODO: generalize
stripSurroundings :: GhcDateVersion -> RelativePath -> FileName
stripSurroundings (GhcDateVersion ghcDateVersion) =
    FileName
  . reverse
  . drop (textLength ".tar.xz")
  . reverse
  . drop (textLength $ "./" <> ghcDateVersion <> "-")
  . relativePathText

err :: Text -> IO ()
err s = TIO.hPutStrLn Sys.stderr ("***** " <> s)

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
            Nothing -> fail $ "Missing sha1 for file: " <> unpack (relativePathText relPath)
          pure $ Just $ GhcSetupInfo
            { ghcSetupInfoArch = arch
            , ghcSetupInfoVersion = ghcVersion
            , ghcSetupInfoUrl = toUrl baseUrl relPath
            , ghcSetupInfoSha256 = sha256
            , ghcSetupInfoSha1 = sha1
            }
        UnrecognizedFileName -> fail $
          "Encountered unrecognized file name: " <> unpack (relativePathText relPath)
        where
          file = stripSurroundings ghcDateVersion relPath
  parseInfoMaybes <- mapM parseInfo $ Map.toAscList sha256s
  pure $ map ghcSetupInfoUrlCorrection $ catMaybes parseInfoMaybes

printGhcSetupInfo :: Int -> GhcSetupInfo -> IO ()
printGhcSetupInfo indent info = do
  let Arch arch = ghcSetupInfoArch info
      Url url = ghcSetupInfoUrl info
      GhcVersion ver = ghcSetupInfoVersion info
      Sha256Sum sha256 = ghcSetupInfoSha256 info
      Sha1Sum sha1 = ghcSetupInfoSha1 info
      indentText = Text.replicate indent " "
  putStrLn $ indentText <> arch <> ":"
  putStrLn $ indentText <> "    " <> ver <> ":"
  putStrLn $ indentText <> "        url: \"" <> url <> "\""
  putStrLn $ indentText <> "        sha1: " <> sha1
  putStrLn $ indentText <> "        sha256: " <> sha256

-- Decomissioned
maybePrintExtraInfo :: Int -> [GhcSetupInfo] -> IO ()
maybePrintExtraInfo indent ghcSetupInfos = do
  let srcArch = "linux64-tinfo6" :: Arch
      destArch = "linux64-tinfo6-nopie" :: Arch
  let isDupeSrc info = ghcSetupInfoArch info == srcArch
      isDupeDest info = ghcSetupInfoArch info == destArch
      extraInfo = case find isDupeSrc ghcSetupInfos of
        Just srcInfo -> case find isDupeDest ghcSetupInfos of
          Nothing -> Just $ srcInfo { ghcSetupInfoArch = destArch }
          Just _ -> Nothing
        Nothing -> Nothing
  case extraInfo of
    Just i -> do
      let indentText = Text.replicate indent " "
      putStrLn $ indentText <> "# not needed for stack-1.7+; copy of " <> archText srcArch
      printGhcSetupInfo indent i
    Nothing -> pure ()

printCoda :: GhcDateVersion -> IO ()
printCoda (GhcDateVersion ghcDateVersion) = do
  putStrLn ""
  putStrLn $ "resolver: " <> ghcDateVersion
  putStrLn $ "compiler: " <> ghcDateVersion
  putStrLn "compiler-check: match-exact"
  putStrLn "packages: []"

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
        _ -> ( GhcDateVersion "ghc-8.6.2"
             , GhcVersion "8.6.2"
             , BaseUrl "https://downloads.haskell.org/~ghc/8.6.2/"
             )
  -- let ghcDateVersion = GhcDateVersion "ghc-8.6.2"
  --     ghcVersion = GhcVersion "8.6.2"
  --     baseUrl = BaseUrl "https://downloads.haskell.org/~ghc/8.6.2/"
  ghcSetupInfos <- loadGhcSetupInfo ghcDateVersion ghcVersion baseUrl
  putStrLn "setup-info:"
  putStrLn "  ghc:"
  mapM_ (printGhcSetupInfo 4) ghcSetupInfos
  printCoda ghcDateVersion
  pure ()


-- the old, deprecated way
main1 :: IO ()
main1 = do
  let ghcDateVersion = GhcDateVersion "ghc-8.6.2"
      ghcVersion = GhcVersion "8.6.2"
      baseUrl = BaseUrl "https://downloads.haskell.org/~ghc/8.6.2/"
  req <- HTTP.parseRequest (unpack (baseUrlText baseUrl) <> "/SHA256SUMS")
  res <- HTTP.httpBS req

  putStrLn "setup-info:"
  putStrLn "  ghc:"
  let textBody = decodeUtf8 $ HTTP.getResponseBody $ res
  F.for_ (Text.lines textBody) $ \line -> case Text.words line of
    [sha256Text, pathText] -> do
      let path = RelativePath pathText
          file = stripSurroundings ghcDateVersion path
          sha256 = Sha256Sum sha256Text
      case parseSystemName file of
        -- I don't really understand this special case
        -- but it seems like these two fedora cases use the same tarball
        FileForArch "linux64-tinfo-nopie" -> do
          printSection ghcDateVersion baseUrl "linux64-tinfo" path sha256
          printSection ghcDateVersion baseUrl "linux64-tinfo-nopie" path sha256
        FileForArch target -> do
          printSection ghcDateVersion baseUrl target path sha256
        UnrecognizedFileName -> do
            err $ "Failed system name lookup: " <> fileNameText file
        ShouldSkipFile -> return ()
    _ -> err $ "Unexpected line: " <> line

  putStrLn ""
  putStrLn $ "resolver: " <> ghcDateVersionText ghcDateVersion
  putStrLn $ "compiler: " <> ghcDateVersionText ghcDateVersion
  putStrLn "compiler-check: match-exact"
  putStrLn "packages: []"

-- Also the old, deprecated way
printSection :: GhcDateVersion -> BaseUrl -> Arch -> RelativePath -> Sha256Sum -> IO ()
printSection (GhcDateVersion ghcDateVersion) (BaseUrl baseUrl) (Arch target) (RelativePath path) (Sha256Sum sha256) = do
  putStrLn $ "    " <> target <> ":"
  putStrLn $ "      " <> (drop (textLength "ghc-") ghcDateVersion) <> ":"
  putStrLn $ "        url: " <> baseUrl <> drop (textLength "./") path
  putStrLn $ "        sha256: " <> sha256
