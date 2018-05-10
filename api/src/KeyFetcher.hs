module KeyFetcher (fetchAndCacheJson) where

import           Control.Applicative          ((<|>))
import           Control.Concurrent
    (MVar, putMVar, threadDelay, tryTakeMVar)
import           Data.Aeson                   (FromJSON, eitherDecode)
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy.Char8   as L8
import           Data.Char                    (isDigit)
import           Data.Maybe                   (listToMaybe)
import           Data.Monoid                  ((<>))
import           Data.Time.Clock.POSIX
    (POSIXTime, getPOSIXTime, utcTimeToPOSIXSeconds)
import           Data.Time.Format             (defaultTimeLocale, parseTimeM)
import           Network.HTTP.Client          (Request, getUri)
import           Network.HTTP.Simple          hiding (Proxy, Request)
import           Network.HTTP.Types.Header    (hCacheControl, hExpires)
import           Text.ParserCombinators.ReadP

-- Fetches a remote JSON document, refreshing it as dictated by its caching
-- headers. This function loops indefinitely and will never return.
--
-- Based heavily on https://stackoverflow.com/a/32739071/379639
fetchAndCacheJson :: FromJSON a => Request -> MVar a -> IO ()
fetchAndCacheJson uri mvar = do
  response <- httpLBS uri

  if getResponseStatusCode response == 200 then
    do
      t <- getPOSIXTime

      let body = getResponseBody response

      -- Default to 60 second refresh if no or invalid caching headers.
      let expiresInSeconds = maybe 60 id (computeExpireTime t response)

      case eitherDecode body of
        Right jwkData -> do
          -- Regardless of whether the mvar is empty (first run) or full, we need
          -- to empty it since putMVar will block if non-empty. This code is only
          -- safe because this thread is the only publisher.
          tryTakeMVar mvar
          putMVar mvar jwkData

          putStrLn $ "Fetched " <> (show . getUri) uri <>
                     ", caching for " <> show expiresInSeconds <> "s"
        Left e -> do
          putStrLn $ "Invalid JSON in response, retrying in " <> show expiresInSeconds <>
                     "s: " <> e

      threadDelay (seconds expiresInSeconds)
  else
    do
      putStrLn $ "Error fetching " <> (show . getUri) uri <>
                 ", retrying in 1s: " <> (show . getResponseStatus $ response)
      threadDelay (seconds 1)

  fetchAndCacheJson uri mvar

seconds x = x * 1000000

computeExpireTime :: POSIXTime -> Network.HTTP.Simple.Response L8.ByteString -> Maybe Int
computeExpireTime now rs =
    let hs              = getResponseHeaders rs
        expires         = do    e <- lookupHeader hExpires hs
                                t <- parseTimeM True defaultTimeLocale "%a, %e %b %Y %T %Z" (B8.unpack e)
                                return . fromIntegral . round $ (utcTimeToPOSIXSeconds t - now)
        cachecontrol    = do    c <- lookupHeader hCacheControl hs
                                d <- readMaxAge $ B8.unpack c
                                return $ d
    in  cachecontrol <|> expires

readMaxAge :: String -> Maybe Int
readMaxAge = fmap fst . listToMaybe . readP_to_S p
    where p = (string "max-age=" >> read <$> munch isDigit) +++ (get >>= const p)

lookupHeader h = listToMaybe . map snd . filter ((h==) . fst)
