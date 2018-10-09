{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.ClientCredentialsRequest where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text (Text)

import           GHC.Generics

data ClientCredentialsRequest = ClientCredentialsRequest 
  { grant_type :: Text
  , redirect_uri :: Maybe Text
  , client_id  :: Text -- Ideally ByteString (no ToJSON)
  , client_secret :: Text -- Ideally ByteString (no ToJSON)
  , audience :: Text
  } deriving (Eq, Show, Generic)
instance ToJSON ClientCredentialsRequest where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON ClientCredentialsRequest where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

mkClientCredentialsRequest :: Text -> Text -> Text -> ClientCredentialsRequest
mkClientCredentialsRequest = ClientCredentialsRequest "client_credentials" Nothing

mkAuthorizationCodeRequest :: Text -> Text -> Text -> Text -> ClientCredentialsRequest
mkAuthorizationCodeRequest redirectUrl = ClientCredentialsRequest "authorization_code" (Just redirectUrl)