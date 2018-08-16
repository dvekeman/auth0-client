{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.AppMetadata where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.ByteString (ByteString)
import           Data.Text (Text)

import           GHC.Generics

data AppMetadata = AppMetadata
  { app_id :: Text
  } deriving (Eq, Show, Generic)
instance ToJSON AppMetadata
instance FromJSON AppMetadata

 
