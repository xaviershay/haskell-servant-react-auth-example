{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- Servant API definition and implementation. In a larger project,
-- implementation would likely be split out into separate files.
module Api where

import qualified Data.Text                        as T
import           Servant
import           Servant.Foreign
import           Servant.Server.Experimental.Auth (AuthServerData)

import Types

-- Provide the missing HasForeign instance for AuthProtect, such that it is
-- compatible with JS generation and servant-options. See
--
-- * https://github.com/sordina/servant-options/issues/2
-- * https://github.com/haskell-servant/servant-auth/issues/8
instance forall lang ftype api.
    ( HasForeign lang ftype api
    , HasForeignType lang ftype T.Text
    )
  => HasForeign lang ftype (AuthProtect "google-jwt" :> api) where
  type Foreign ftype (AuthProtect "google-jwt" :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) req
    where
      req = subR{ _reqHeaders = HeaderArg arg : _reqHeaders subR }
      arg = Arg
        { _argName = PathSegment "Authorization"
        , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy T.Text)
        }

-- Associate the Account type with any route tagged "google-jwt".
type instance AuthServerData (AuthProtect "google-jwt") = Account

-- The main Servant API type.
type MyAPI =
  -- Routes that need to be protected are tagged with AuthProtect
  AuthProtect "google-jwt" :> "email" :> Get '[JSON] T.Text

  -- Unprotected routes are allowed
  :<|> "unprotected" :> Get '[JSON] T.Text

myApiProxy :: Proxy MyAPI
myApiProxy = Proxy

myApi :: Server MyAPI
myApi =
  -- Protected routes will only be called if a valid account was present.
  (\account -> return (acctEmail account))

  -- Unprotected routes function per normal
  :<|> return "unprotected"
