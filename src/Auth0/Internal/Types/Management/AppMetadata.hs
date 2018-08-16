{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.AppMetadata where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text (Text)

import           GHC.Generics

data AppMetadata = AppMetadata
  { app_id :: Text
  } deriving (Eq, Show, Generic)
instance ToJSON AppMetadata where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON AppMetadata where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }
