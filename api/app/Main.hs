{-# LANGUAGE OverloadedStrings   #-}

module Main where

import           Control.Concurrent
import           Control.Monad                          (when)
import qualified Data.Text as T
import qualified Data.Text.IO                           as T
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Servant.Options
import           Servant
import qualified System.Posix.Signals                   as S

import Api
import Auth
import JsGeneration
import KeyFetcher
import Types

-- Construct the Servant application.
app :: AppConfig -> Application
app config =
  let logger = case cnfEnv config of
                 Development -> logStdoutDev
                 Production  -> logStdout
               in

  logger $
  cors (const . Just $ corsPolicy) $ -- Generate appropriate CORS headers
  provideOptions myApiProxy $        -- Generate OPTIONS handlers for routes
  serveWithContext myApiProxy (serverContext config) myApi

  where
    -- Make our auth handler available as a context. It will be run for routes
    -- tagged with AuthProtect. See comments in Auth module.
    --
    -- http://haskell-servant.readthedocs.io/en/stable/tutorial/Authentication.html#generalized-authentication-in-action
    serverContext config = Auth.handler config :. EmptyContext

    -- Need to explictly allow needed extra headers through CORS.
    corsPolicy = simpleCorsResourcePolicy
                   { corsRequestHeaders = [ "authorization", "content-type" ]
                   }

main :: IO ()
main = do
  clientId <- T.strip <$> T.readFile "oauth-client-id"
  config   <- mkConfig clientId

  -- Spawn a background thread to fetch and keep current Google's public JWK,
  -- for verifying JWTs.
  threadId <- forkIO (fetchAndCacheJson "https://www.googleapis.com/oauth2/v3/certs" (cnfJwk config))
  tid <- myThreadId

  when
    (cnfEnv config == Development)
    $ do
      -- Ensure that threads are cleaned up when developing in GHCI. Kill thread is
      -- dangerous, but it's only for dev mode so don't care. In production,
      -- threads will be killed by default when the process is killed.
      S.installHandler S.keyboardSignal
        (S.Catch $ killThread threadId >> killThread tid)
        Nothing

      -- Write out the most current version of the JS API for development. For
      -- production deployment, the frontend should be packaged up separately
      -- (which would likely require a separate code path dedicated to only
      -- writing out the JS and not serving).
      --
      -- Alternatively, you consider serving up the JS API definitions directly
      -- from the API server. I haven't thought too much about which approach
      -- would be better.
      writeJsClient "http://localhost:8000" "../frontend/src/ApiFunctions.js"

  -- Don't begin serving until a JWK has been fetched. In production code it
  -- may be worth considering caching this key more locally so that start up is
  -- not blocked by an internet request.
  --
  -- Alternatively or in addition, cnfJwk could be made a Maybe and requests
  -- could immediately 500 if not present. This would make more sense if there
  -- were a number of unauthenticated routes.
  readMVar (cnfJwk config)

  putStrLn "Serving API on localhost:8000"
  run 8000 (app config)

focus = main
