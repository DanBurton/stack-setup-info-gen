module Stack.Setup.Info.Fetch where

import ClassyPrelude
import qualified Network.HTTP.Client as Client
import Control.Concurrent (threadDelay)
import qualified Data.Map as Map

import Stack.Setup.Info.Types

data CachingStrategy
  = LocalOnly
  | CacheLocal Client.Manager
  | NoCache Client.Manager

data GhcTextFile
  = Sha1Sums
  | Sha256Sums
  | ContentLengths

toFileName :: GhcTextFile -> FileName
toFileName Sha1Sums = "SHA1SUMS.txt"
toFileName Sha256Sums = "SHA256SUMS.txt"
toFileName ContentLengths = "CONTENT_LENGTHS.txt"

-- Sadly requesting "index.html" doesn't produce this file
contentLengthsHtmlFileName :: FileName
contentLengthsHtmlFileName = "index.html"

toServerFileName :: GhcTextFile -> RelativePath
toServerFileName = \ case
  Sha1Sums -> "./SHA1SUMS"
  Sha256Sums -> "./SHA256SUMS"
  ContentLengths -> "./" -- but see contentLengthsHtmlFileName

ghcTextFileToUrl :: GhcDisplayVersion -> GhcTextFile -> Url
ghcTextFileToUrl ghcDisplayVersion ghcTextFile =
  toUrl ghcDisplayVersion (toServerFileName ghcTextFile)

-- TODO: add caching in "input" folder
fetchTextFile :: GhcDisplayVersion -> GhcTextFile -> CachingStrategy -> IO LText
fetchTextFile ghcDisplayVersion ghcTextFile = \ case
  LocalOnly -> do fail "TODO LocalOnly"
  CacheLocal _manager -> do fail "TODO CacheLocal"
  NoCache manager -> do
    let url = ghcTextFileToUrl ghcDisplayVersion ghcTextFile
    text <- requestTextFile url manager
    pure $ case ghcTextFile of
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

loadTextFile :: ShaFile -> (Text -> shaSum) -> GhcDisplayVersion -> Client.Manager
         -> IO (Map RelativePath shaSum)
loadTextFile (ShaFile shaFile) mkSha (GhcDisplayVersion ghcDisplayVersion) manager = do
  req <- Client.parseRequest $ unpack $ baseBaseUrl <> ghcDisplayVersion <> "/" <> shaFile
  res <- politelyRequest Client.httpLbs req manager
  let textBody = decodeUtf8 $ Client.responseBody $ res
      bodyLines = lines textBody
  pairs <- mapM lineToShaPair bodyLines
  pure $ Map.fromList pairs
  where
    lineToShaPair line = case words line of
      [shaText, pathText] -> pure
        ( RelativePath $ toStrict pathText
        , mkSha $ toStrict shaText
        )
      _ -> fail $ "SHA file line was not in expected format"


loadSha256s :: GhcDisplayVersion -> Client.Manager -> IO (Map RelativePath Sha256Sum)
loadSha256s = loadTextFile "SHA256SUMS" Sha256Sum

loadSha1s :: GhcDisplayVersion -> Client.Manager -> IO (Map RelativePath Sha1Sum)
loadSha1s = loadTextFile "SHA1SUMS" Sha1Sum

loadContentLengths :: GhcDisplayVersion -> Client.Manager -> IO (Map Url ContentLength)
loadContentLengths gdv@(GhcDisplayVersion ghcDisplayVersion) manager = do
  req <- Client.parseRequest $ unpack $ baseBaseUrl <> ghcDisplayVersion <> "/"
  res <- politelyRequest Client.httpLbs req manager
  let textBody = decodeUtf8 $ Client.responseBody $ res
      -- It may seem silly to do it this way now.
      -- The point is to eventually cache the intermediate file so that CONTENT_LENGTHS.txt
      -- is a simple, readable fine in the same format as the SHA sum files.
      textBody' = writeContentLengths $ scrapeContentLengths textBody
      bodyLines = lines textBody'
  pairs <- mapM lineToShaPair bodyLines
  pure $ Map.fromList pairs
  where
    lineToShaPair line = case words line of
      [contentLengthText, pathText] -> case readMay $ unpack contentLengthText of
        Just contentLength -> pure
          ( toUrl gdv (RelativePath $ toStrict pathText)
          , ContentLength $ contentLength
          )
        _ -> fail $ "CONTENT_LENGTHS.txt file line was not in expected format"
      _ -> fail $ "CONTENT_LENGTHS.txt file line was not in expected format"
