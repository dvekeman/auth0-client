{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.Connection where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text (Text)

import           Auth0.Internal.Types.Management.ConnectionOptions
import           Auth0.Internal.Types.Management.ConnectionMetadata

import           GHC.Generics

data Connection = Connection
  { name :: Maybe Text 
  , options :: Maybe ConnectionOptions
  , id :: Maybe Text
  , strategy :: Maybe Text
  , realms :: [Text]
  , is_domain_connection :: Maybe Bool
  , metadata :: Maybe ConnectionMetadata
  } deriving (Eq, Show, Generic)
instance ToJSON Connection where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON Connection where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }
