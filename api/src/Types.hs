-- Common data types
module Types where

import           Control.Concurrent (MVar, newEmptyMVar)
import           Crypto.JWT         (JWKSet)
import qualified Data.Text          as T

data Account = Account
  { acctEmail :: T.Text
  }

data Environment = Development | Production deriving (Show, Eq)

data AppConfig = AppConfig
  { cnfJwk :: MVar JWKSet
  , cnfEnv :: Environment
  , cnfOauthClientId :: T.Text
  }

mkConfig :: T.Text -> IO AppConfig
mkConfig clientId = do
  jwkVar <- newEmptyMVar

  return $ AppConfig { cnfJwk = jwkVar
                     , cnfEnv = Development
                     , cnfOauthClientId = clientId
                     }
