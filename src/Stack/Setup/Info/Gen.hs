module Stack.Setup.Info.Gen
  ( mainWithArgs
  -- TODO: separate mainWithArgs into yet another module
  , stripSurroundings
  ) where

-- TODO: move usage
-- usage: stack run ghc-8.6.1-beta1

import Data.Semigroup ((<>))

import ClassyPrelude
import qualified Network.HTTP.Client.TLS as TLS
-- TODO: use this again
-- import qualified System.IO as Sys
-- TODO: use this again
-- import qualified Data.Text.IO as TIO
import qualified Data.Map as Map

import Stack.Setup.Info.Types
import Stack.Setup.Info.Fetch

data GhcSetupInfo = GhcSetupInfo
  { ghcSetupInfoArch :: Arch
  , ghcSetupInfoGhcVersion :: GhcVersion
  , ghcSetupInfoUrl :: Url
  , ghcSetupInfoContentLength :: ContentLength
  , ghcSetupInfoSha256 :: Sha256Sum
  , ghcSetupInfoSha1 :: Sha1Sum
  } deriving (Eq, Ord, Show)

shouldSkipFile :: FileName -> Bool
shouldSkipFile "src" = True
shouldSkipFile "testsuite" = True
shouldSkipFile "windows-extra-src" = True
-- The following cases clearly show that this is the wrong way to go about this
shouldSkipFile "x86_64-deb8-linux" = True -- also not sure how to disambiguate from deb8
shouldSkipFile "x86_64-deb8-linux-dwarf" = True -- not sure how to disambiguate from deb9
shouldSkipFile "x86_64-deb9-linux-dwarf" = True -- also not sure how to disambiguate from deb9
shouldSkipFile "x86_64-centos7-linux" = True -- Not sure what the systemNameMapping is for this
shouldSkipFile _ = False

systemNameMapping :: SystemName -> Maybe Arch
systemNameMapping "i386-deb8-linux" = Just "linux32"
systemNameMapping "i386-deb9-linux" = Just "linux32"
systemNameMapping "i386-unknown-mingw32" = Just "windows32"
systemNameMapping "i386-unknown-mingw32-win10" = Just "windows32"
systemNameMapping "x86_64-apple-darwin" = Just "macosx"
systemNameMapping "x86_64-deb9-linux" = Just "linux64"
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

-- TODO: generalize?
stripSurroundings :: GhcDisplayVersion -> GhcVersion -> Url -> FileName
stripSurroundings (GhcDisplayVersion ghcDisplayVersion) (GhcVersion ghcVersion) (Url url) =
    FileName
  . dropSuffixLength ".tar.xz"
  . dropPrefixLength ("/ghc-" <> ghcVersion <> "-")
  . dropPrefixLength ghcDisplayVersion
  . dropPrefixLength baseBaseUrl
  $ url

-- TODO: use this again
-- err :: Text -> IO ()
-- err s = TIO.hPutStrLn Sys.stderr ("***** " <> s)

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

-- TODO: warn about errors, gracefully degrade rather than fatally crash
loadGhcSetupInfo :: GhcVersion -> GhcDisplayVersion -> CachingStrategy
                 -> IO [GhcSetupInfo]
loadGhcSetupInfo ghcVersion ghcDisplayVersion strat = do
  logg "loading sha1s"
  sha1s <- loadSha1s ghcDisplayVersion strat
  logg "loading sha256s"
  sha256s <- loadSha256s ghcDisplayVersion strat
  logg "loading contentLengths"
  contentLengths <- loadContentLengths ghcDisplayVersion strat
  let parseInfo :: (Url, Sha256Sum) -> IO (Maybe GhcSetupInfo)
      parseInfo (url, sha256) = case parseSystemName file of
        ShouldSkipFile -> pure $ Nothing
        FileForArch arch -> do
          sha1 <- case Map.lookup url sha1s of
            Just s -> pure s
            Nothing -> tfail $ "Missing sha1 for file: " <> urlText
          logg $ "discovering contentLength for " <> urlText
          contentLength <- discoverContentLength url contentLengths
          logg $ "finished for " <> urlText
          pure $ Just $ GhcSetupInfo
            { ghcSetupInfoArch = arch
            , ghcSetupInfoGhcVersion = ghcVersion
            , ghcSetupInfoUrl = url
            , ghcSetupInfoContentLength = contentLength
            , ghcSetupInfoSha256 = sha256
            , ghcSetupInfoSha1 = sha1
            }
        UnrecognizedFileName -> tfail $
          "Encountered unrecognized file name: " <> urlText
        where
          Url urlText = url
          file = stripSurroundings ghcDisplayVersion ghcVersion url
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

discoverContentLength :: Url -> Map Url ContentLength -> IO ContentLength
discoverContentLength url coll = case lookup url coll of
  Just cl -> pure cl
  Nothing -> let Url urlt = url in tfail $ "Couldn't find cl for " <> urlt

-- TODO: better types
-- TODO: implement this by looking at a SHA file
discoverDateVer :: GhcDisplayVersion -> IO GhcVersion
discoverDateVer "8.6.1-beta1" = pure "8.6.0.20180810"
discoverDateVer "8.8.1-alpha1" = pure "8.8.0.20190424"
discoverDateVer (GhcDisplayVersion t) = tfail $ "Could not discover ghc version at: " <> t

-- TODO: reduce code duplication
guessGhcVerReps :: Text -> IO (GhcVersion, GhcDisplayVersion)
guessGhcVerReps text = case splitElem '-' text of
  [ver] -> pure
    ( GhcVersion ver
    , GhcDisplayVersion ver
    )
  ["ghc", ver] -> pure
    ( GhcVersion ver
    , GhcDisplayVersion ver
    )
  ["ghc", prospectiveVer, tag]
    | any (`isPrefixOf` tag) ["alpha", "beta", "rc"] -> do
        let displayVersion = GhcDisplayVersion $ prospectiveVer <> "-" <> tag
        ghcVersion <- discoverDateVer displayVersion
        pure
          ( ghcVersion
          , displayVersion
          )
  [prospectiveVer, tag]
    | any (`isPrefixOf` tag) ["alpha", "beta", "rc"] -> do
        let displayVersion = GhcDisplayVersion $ prospectiveVer <> "-" <> tag
        ghcVersion <- discoverDateVer displayVersion
        pure
          ( ghcVersion
          , displayVersion
          )
  _ -> tfail $ "Could not understand this ghc version: " <> text

-- TODO: error message that suggests using --cache-local if the default of --local-only fails.
mainWithArgs :: [Text] -> IO ()
mainWithArgs args = do
  (verArg, strat) <- case args of
    [arg] -> pure (arg, LocalOnly)
    [arg, "--local-only"] -> pure (arg, LocalOnly)
    [arg, "--cache-local"] -> do
      manager <- TLS.newTlsManager
      pure (arg, CacheLocal manager)
    [arg, "--no-cache"] -> do
      manager <- TLS.newTlsManager
      pure (arg, NoCache manager)
    _ -> tfail $ "Too many args, expected only 1, got this: " <> tshow args
  (ghcVersion, ghcDateVersion) <- guessGhcVerReps verArg
  ghcSetupInfos <- loadGhcSetupInfo ghcVersion ghcDateVersion strat
  putStrLn "# This file was generated:"
  putStrLn "# https://github.com/DanBurton/stack-setup-info-gen/"
  putStrLn "setup-info:"
  putStrLn "  ghc:"
  mapM_ (printGhcSetupInfo 4) ghcSetupInfos
  printCoda ghcVersion
  pure ()

logg :: Text -> IO ()
-- logg = putStrLn -- TODO stderr
logg _ = pure ()
