{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.ClientCredentialsRequest where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.ByteString (ByteString)
import           Data.Text (Text)

import           GHC.Generics

data ClientCredentialsRequest = ClientCredentialsRequest 
  { grant_type :: Text
  , client_id  :: Text -- Ideally ByteString (no ToJSON)
  , client_secret :: Text -- Ideally ByteString (no ToJSON)
  , audience :: Text
  } deriving (Eq, Show, Generic)
instance ToJSON ClientCredentialsRequest
instance FromJSON ClientCredentialsRequest

defaultClientCredentialsRequest :: Text -> Text -> Text -> ClientCredentialsRequest
defaultClientCredentialsRequest = ClientCredentialsRequest "client_credentials"