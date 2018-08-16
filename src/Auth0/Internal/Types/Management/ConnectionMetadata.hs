{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.ConnectionMetadata where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)

import           GHC.Generics

data ConnectionMetadata = ConnectionMetadata
  { 
  } deriving (Eq, Show, Generic)
instance ToJSON ConnectionMetadata where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON ConnectionMetadata where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }
