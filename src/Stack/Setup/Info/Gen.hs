{-# LANGUAGE ViewPatterns #-}
module Stack.Setup.Info.Gen
  ( mainWithArgs
  -- TODO: separate mainWithArgs into yet another module
  , stripSurroundings
  ) where

-- TODO: move usage
-- usage: stack run ghc-8.6.1-beta1

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
shouldSkipFile "x86_64-deb8-linux" = True -- also not sure how to disambiguate from deb9
shouldSkipFile "x86_64-deb8-linux-dwarf" = True -- not sure how to disambiguate from deb9
shouldSkipFile "x86_64-deb9-linux-dwarf" = True -- also not sure how to disambiguate from deb9
shouldSkipFile "x86_64-deb10-linux" = True -- also not sure how to disambiguate from deb9
shouldSkipFile "x86_64-deb10-linux-dwarf" = True -- also not sure how to disambiguate from deb9
shouldSkipFile "x86_64-deb11-linux" = True -- at this point I'm just cargo culting myself from years ago
shouldSkipFile "x86_64-deb12-linux" = True -- at this point I'm just cargo culting myself from years ago
shouldSkipFile "armv7-deb9-linux" = True -- Does stack support arm yet?
shouldSkipFile "armv7-deb10-linux" = True -- Does stack support arm yet?
shouldSkipFile "x86_64-alpine3_12-linux" = True -- I don't know what to do with this
shouldSkipFile "x86_64-alpine3_18-linux" = True -- I don't know what to do with this
shouldSkipFile "x86_64-alpine3.10-linux-integer-simple" = True -- I don't know what to do with this
shouldSkipFile "x86_64-unknown-mingw32-integer-simple" = True -- I don't know what to do with this
shouldSkipFile "x86_64-alpine3_12-linux-static" = True -- I don't know what to do with this
shouldSkipFile "x86_64-alpine3_12-linux-static-int_native" = True -- I don't know what to do with this
shouldSkipFile "aarch64-apple-darwin" = True -- idk what stack calls this Arch
shouldSkipFile "aarch64-alpine3_18-linux" = True -- idk what stack calls this Arch
shouldSkipFile "x86_64-fedora33-linux-dwarf" = True -- I am doing this because I'm lazy
shouldSkipFile "x86_64-fedora33-linux" = True -- I am doing this because I'm lazy
shouldSkipFile "x86_64-fedora38-linux" = True -- I am doing this because I'm lazy
shouldSkipFile "x86_64-unknown-mingw32-int_native" = True -- I am diong this because I'm lazy
shouldSkipFile "x86_64-rocky8-linux" = True -- I am doing this because I'm lazy
shouldSkipFile (FileName (stripSuffix ".zip" -> Just _)) = True -- don't need .zip when we have .tar.xz ?
shouldSkipFile (FileName (stripSuffix ".tar.lz" -> Just _)) = True -- don't need .tar.lz when we have .tar.xz ?
shouldSkipFile (FileName (stripSuffix ".tar.bz2" -> Just _)) = True -- don't need .tar.bz2 when we have .tar.xz ?
shouldSkipFile _ = False

systemNameMapping :: SystemName -> Maybe Arch
systemNameMapping "i386-deb8-linux" = Just "linux32"
systemNameMapping "i386-deb9-linux" = Just "linux32"
systemNameMapping "i386-deb10-linux" = Just "linux32"
systemNameMapping "i386-unknown-mingw32" = Just "windows32"
systemNameMapping "i386-unknown-mingw32-win10" = Just "windows32"
systemNameMapping "x86_64-apple-darwin" = Just "macosx"
systemNameMapping "x86_64-deb9-linux" = Just "linux64"
systemNameMapping "x86_64-ubuntu18_04-linux" = Just "linux64"
systemNameMapping "x86_64-ubuntu20_04-linux" = Just "linux64"
systemNameMapping "x86_64-unknown-mingw32" = Just "windows64"
systemNameMapping "x86_64-unknown-mingw32-win10" = Just "windows64"
systemNameMapping "x86_64-fedora27-linux" = Just "linux64-tinfo6"
systemNameMapping "x86_64-unknown-linux" = Just "linux64"
systemNameMapping "x86_64-darwin" = Just "macosx"
systemNameMapping "x86_64-fedora-linux" = Just "linux64-tinfo6"
systemNameMapping "aarch64-deb8-linux" = Just "linux-aarch64"
systemNameMapping "aarch64-deb9-linux" = Just "linux-aarch64"
systemNameMapping "aarch64-deb10-linux" = Just "linux-aarch64"
systemNameMapping "aarch64-deb11-linux" = Just "linux-aarch64"
systemNameMapping "x86_64-portbld-freebsd" = Just "freebsd64"
systemNameMapping "x86_64-unknown-freebsd" = Just "freebsd64"
systemNameMapping "i386-portbld-freebsd" = Just "freebsd32"
systemNameMapping "x86_64-centos7-linux" = Just "linux64-gmp4"
systemNameMapping _ = Nothing

-- TODO: generalize?
stripSurroundings :: GhcDisplayVersion -> GhcVersion -> Url -> FileName
stripSurroundings gdv@(GhcDisplayVersion ghcDisplayVersion) (GhcVersion ghcVersion) (Url url) =
    FileName
  . tryDropASuffix [".tar.xz"]
  . dropPrefixLength ("/ghc-" <> ghcVersion <> "-")
  . dropPrefixLength ghcDisplayVersion
  . dropPrefixLength (baseBaseUrl gdv)
  $ url

-- TODO: use this again
-- err :: Text -> IO ()
-- err s = TIO.hPutStrLn Sys.stderr ("***** " <> s)

asSystemName :: FileName -> SystemName
asSystemName (FileName fn) = SystemName fn

fnText :: FileName -> Text
fnText (FileName fn) = fn

data SystemNameParse =
    ShouldSkipFile
  | UnrecognizedFileName
  | FileForArch Arch
  deriving Show

parseSystemName :: FileName -> SystemNameParse
parseSystemName fileName
  | shouldSkipFile fileName = ShouldSkipFile
  | otherwise = case systemNameMapping (asSystemName fileName) of
      Just arch -> FileForArch arch
      Nothing -> UnrecognizedFileName

suffixIn :: Url -> [Text] -> Bool
suffixIn (Url url) suffixes = any (`isSuffixOf` url) suffixes

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
      parseInfo (url, _sha256) | url `suffixIn` [".tar.lz"] = pure Nothing
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
            <> "\n (snipped as " <> fnText file <> " )"
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
  Nothing -> let Url urlt = url in case urlt of
    (stripSuffix "x86_64-unknown-mingw32.tar.xz" -> Just _) -> do
      -- TODO: make noise that we are ignoring this
      return $ ContentLength 0
    _ -> tfail $ "Couldn't find cl for " <> urlt

-- TODO: better types
-- TODO: implement this by looking at a SHA file
discoverDateVer :: GhcDisplayVersion -> IO GhcVersion
discoverDateVer "8.6.1-beta1" = pure "8.6.0.20180810"
discoverDateVer "8.8.1-alpha1" = pure "8.8.0.20190424"
discoverDateVer "8.8.1-alpha2" = pure "8.8.0.20190613"
discoverDateVer "8.8.1-rc1" = pure "8.8.0.20190721"
discoverDateVer "8.8.2-rc1" = pure "8.8.1.20191211"
discoverDateVer "8.10.1-alpha1" = pure "8.10.0.20191121"
discoverDateVer "8.10.1-alpha2" = pure "8.10.0.20191210"
discoverDateVer "8.10.1-rc1" = pure "8.10.0.20200123"
discoverDateVer "9.0.1-alpha1" = pure "9.0.0.20200925"
discoverDateVer "9.0.1-rc1" = pure "9.0.0.20201227"
discoverDateVer "9.2.1-alpha1" = pure "9.2.0.20210331"
discoverDateVer "9.2.1-alpha2" = pure "9.2.0.20210422"
discoverDateVer "9.2.1-rc1" = pure "9.2.0.20210821"
discoverDateVer "9.4.1-rc1" = pure "9.4.0.20220721"
discoverDateVer "9.10.1-alpha1" = pure "9.10.0.20240313"
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
  (verArg, cachingStrategy) <- case args of
    [arg] -> pure (arg, LocalOnly)
    [arg, "--no-fetch-inputs"] -> pure (arg, LocalOnly)
    [arg, "--fetch-inputs"] -> do
      manager <- TLS.newTlsManager
      pure (arg, CacheLocal manager)
    [arg, "--fetch-inputs-with-no-cache"] -> do
      manager <- TLS.newTlsManager
      pure (arg, NoCache manager)
    _ -> tfail $ "Too many args, expected only 1, got this: " <> tshow args
  (ghcVersion, ghcDateVersion) <- guessGhcVerReps verArg
  ghcSetupInfos <- loadGhcSetupInfo ghcVersion ghcDateVersion cachingStrategy
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
