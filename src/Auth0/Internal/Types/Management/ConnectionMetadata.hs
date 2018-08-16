{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.ConnectionMetadata where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.Text (Text)

import           GHC.Generics

data ConnectionMetadata = ConnectionMetadata
  { 
  } deriving (Eq, Show, Generic)
instance ToJSON ConnectionMetadata
instance FromJSON ConnectionMetadata
