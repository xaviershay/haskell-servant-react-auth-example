{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Specific validation functions for handling Google JWTs, as well as a Servant
-- Generalized Authentication handler as documented here:
--
-- http://haskell-servant.readthedocs.io/en/stable/tutorial/Authentication.html#generalized-authentication-in-action
module Auth where

import           Control.Concurrent               (readMVar)
import           Control.Lens                     (at, (^.))
import           Control.Monad.Except
    (MonadError, MonadIO, liftIO, runExceptT, throwError)
import           Crypto.JWT
    ( JWKSet
    , JWTError
    , decodeCompact
    , defaultJWTValidationSettings
    , unregisteredClaims
    , verifyClaims
    )
import           Data.Aeson
    (Result (Error, Success), fromJSON)
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy.Char8       as L8
import           Data.Monoid                      ((<>))
import           Data.String                      (fromString)
import qualified Data.Text                        as T
import           Network.Wai                      (Request, requestHeaders)
import           Servant                          (err401, errBody)
import           Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)

import Types

validateJwt :: (MonadError String m, MonadIO m) => T.Text -> JWKSet -> B8.ByteString -> m Account
validateJwt clientId keyset bearerCreds = do
  -- These settings will verify that the JWT was issued in response to an OAuth
  -- request that used our client ID. That cliend ID is public, but is
  -- restricted to our specific domains that Google will allow authentication
  -- from.
  let config = defaultJWTValidationSettings (== (fromString . T.unpack $ clientId))

  -- "compact" is the name for a base64 encoded JWT. Since the raw
  -- Authorization header is passed in, we need to drop the leading "Bearer "
  -- text before decoding.
  --
  -- See explanation of liftIO . runExceptT in `handler`.
  verifiedJwt <- liftIO . runExceptT $
        decodeCompact (L8.fromStrict $ B8.drop (B8.length "Bearer ") bearerCreds)
    >>= verifyClaims config keyset

  case verifiedJwt of
    Left (e :: JWTError) -> throwError ("Could not verify JWT: " <> show e)
    Right claimset       -> do
      let emailClaim = fromJSON <$> claimset ^. unregisteredClaims ^. at "email"

      email <- (maybeToError "No email claim present" emailClaim) >>= aesonResultToError

      return (Account { acctEmail = email })

-- A servant Generalized Authorization handler. Will 401 unless a valid JWT is present.
handler :: AppConfig -> AuthHandler Request Account
handler config = mkAuthHandler f
  where
    f req = do
      keyset <- liftIO . readMVar $ cnfJwk config

      -- This is more convolute than it needs to be because verifyClaims (in
      -- validateJwt) needs to run MonadTime, which in this case is IO. But I
      -- don't want to use servant specific exception handling for errors (i.e.
      -- using throw401 directly inside validateJwt), so am using runExceptT so
      -- that throwError is captured in an Either.  This same pattern is also
      -- used inside validateJwt.
      --
      -- jose-0.7 provides verifyClaimsAt which means the MonadTime requirement
      -- could be pulled up and this likely simplified.
      account <- liftIO . runExceptT $
            maybeToError "No Authorization header present" (lookupReqHeader "Authorization" req)
        >>= validateJwt (cnfOauthClientId config) keyset

      either throw401 return account

    lookupReqHeader h = lookup h . requestHeaders
    throw401 s = throwError err401 { errBody = L8.pack s }

aesonResultToError :: MonadError String m => Result a -> m a
aesonResultToError (Error e) = throwError e
aesonResultToError (Success x) = return x

maybeToError :: (MonadError e m) => e -> Maybe a -> m a
maybeToError s = maybe (throwError s) (return)
