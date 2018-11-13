module Stack.Setup.Info.Fetch where

import ClassyPrelude
import qualified Network.HTTP.Client as Client
import Control.Concurrent (threadDelay)
import qualified Data.Map as Map
import qualified System.Directory as Directory

import Stack.Setup.Info.Types

data CachingStrategy
  = LocalOnly
  | CacheLocal Client.Manager
  | NoCache Client.Manager

data GhcTextFile
  = Sha1Sums
  | Sha256Sums
  | ContentLengths

toFilePath :: GhcTextFile -> FilePath
toFilePath Sha1Sums = "SHA1SUMS.txt"
toFilePath Sha256Sums = "SHA256SUMS.txt"
toFilePath ContentLengths = "CONTENT_LENGTHS.txt"

-- Sadly requesting "index.html" doesn't produce this file
indexFileName :: FileName
indexFileName = "index.html"

indexFilePath :: GhcDisplayVersion -> FilePath
indexFilePath gdv = ghcTextFileDirName gdv </> "index.html"

toServerFileName :: GhcTextFile -> RelativePath
toServerFileName = \ case
  Sha1Sums -> "./SHA1SUMS"
  Sha256Sums -> "./SHA256SUMS"
  ContentLengths -> "./" -- but see contentLengthsHtmlFileName

ghcTextFileToUrl :: GhcDisplayVersion -> GhcTextFile -> Url
ghcTextFileToUrl ghcDisplayVersion ghcTextFile =
  toUrl ghcDisplayVersion (toServerFileName ghcTextFile)

ghcTextFileToFilePath :: GhcDisplayVersion -> GhcTextFile -> FilePath
ghcTextFileToFilePath gdv gtf =
  ghcTextFileDirName gdv </> toFilePath gtf

fetchLocalTextFile :: GhcDisplayVersion -> GhcTextFile -> IO (Maybe LText)
fetchLocalTextFile gdv gtf = do
  let filePath = ghcTextFileToFilePath gdv gtf
  Directory.doesFileExist filePath >>= \ case
    True -> do
      Just . fromStrict <$> readFileUtf8 filePath
    False -> pure Nothing

fetchRemoteTextFile :: GhcDisplayVersion -> GhcTextFile -> Client.Manager -> IO LText
fetchRemoteTextFile gdv gtf manager = do
  let url = ghcTextFileToUrl gdv gtf
  requestTextFile url manager

ghcTextFileDirName :: GhcDisplayVersion -> FilePath
ghcTextFileDirName (GhcDisplayVersion gdv) = "input" </> unpack gdv

fetchTextFile :: GhcDisplayVersion -> GhcTextFile -> CachingStrategy -> IO LText
fetchTextFile gdv gtf = \ case
  LocalOnly -> fetchLocalTextFile gdv gtf >>= \ case
    Just t -> pure t
    Nothing -> fail $ "couldn't find local file: " <> ghcTextFileToFilePath gdv gtf
  CacheLocal manager -> fetchLocalTextFile gdv gtf >>= \ case
    Just t -> pure t
    Nothing -> do
      let dirName = ghcTextFileDirName gdv
      Directory.createDirectoryIfMissing True dirName
      t <- fetchRemoteTextFile gdv gtf manager
      case gtf of
        ContentLengths -> do
          writeFileUtf8 (indexFilePath gdv) $ toStrict t
          let t' = writeContentLengths $ scrapeContentLengths t
          writeFileUtf8 (ghcTextFileToFilePath gdv gtf) $ toStrict t'
          pure t'
        _ -> do
          writeFileUtf8 (ghcTextFileToFilePath gdv gtf) $ toStrict t
          pure t
  NoCache manager -> do
    let url = ghcTextFileToUrl gdv gtf
    text <- requestTextFile url manager
    pure $ case gtf of
      ContentLengths -> writeContentLengths $ scrapeContentLengths text
      _ -> text

requestTextFile :: Url -> Client.Manager -> IO LText
requestTextFile (Url url) manager = do
  req <- Client.parseRequest $ unpack $ url
  res <- politelyRequest Client.httpLbs req manager
  pure $ decodeUtf8 $ Client.responseBody $ res

politelyRequest
  :: (Client.Request -> Client.Manager -> IO a)
  -> (Client.Request -> Client.Manager -> IO a)
politelyRequest doReq r m = do
  threadDelay 100000 -- politely wait 0.1s between requests
  doReq r m

writeContentLengths :: [(ContentLength, RelativePath)] -> LText
writeContentLengths = (<> "\n") . ointercalate "\n" . map lineToText
  where
    lineToText (ContentLength cl, RelativePath rp) =
      fromStrict $ tshow cl <> "  " <> rp

scrapeContentLengths :: LText -> [(ContentLength, RelativePath)]
scrapeContentLengths t =
  case splitSeq "<pre>" t of
    [_, t'] -> case splitSeq "</pre>" t' of
      [t'', _] -> catMaybes $ map parseLine $ drop 2 $ lines t''
      _ -> [] -- TODO: proper errors?
    _ -> []
  where
    parseLine :: LText -> Maybe (ContentLength, RelativePath)
    parseLine line = case words line of
      [_, href, _, _, contentLength] -> case splitSeq "\"" href of
        [_, file, _] -> case readMay $ unpack contentLength of
          Just cl -> Just
            (ContentLength cl, RelativePath $ toStrict $ "./" <> file)
          Nothing -> Nothing
        _ -> Nothing
      _ -> Nothing

loadTextFile :: GhcTextFile -> (Text -> IO t) -> GhcDisplayVersion -> CachingStrategy
             -> IO (Map Url t)
loadTextFile gtf mkT gdv strat = do
  textBody <- fetchTextFile gdv gtf strat
  pairs <- mapM lineToPair $ lines textBody
  pure $ Map.fromList pairs
  where
    lineToPair line = case words line of
      [datumText, pathText] -> do
        datum <- mkT $ toStrict datumText
        pure
          ( toUrl gdv $ RelativePath $ toStrict pathText
          , datum
          )
      _ -> fail $ "File line was not in expected format"

loadSha256s :: GhcDisplayVersion -> CachingStrategy -> IO (Map Url Sha256Sum)
loadSha256s = loadTextFile Sha256Sums (pure . Sha256Sum)

loadSha1s :: GhcDisplayVersion -> CachingStrategy -> IO (Map Url Sha1Sum)
loadSha1s = loadTextFile Sha1Sums (pure . Sha1Sum)

loadContentLengths :: GhcDisplayVersion -> CachingStrategy -> IO (Map Url ContentLength)
loadContentLengths = loadTextFile ContentLengths $ \ t -> case readMay t of
  Just i -> pure $ ContentLength i
  Nothing -> tfail $ "Could not read text as content length: " <> t
