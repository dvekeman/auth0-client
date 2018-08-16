{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.Connection where

import           Data.Aeson (ToJSON, FromJSON)
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
instance ToJSON Connection
instance FromJSON Connection
