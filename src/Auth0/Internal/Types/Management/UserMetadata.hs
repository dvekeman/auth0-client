{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.UserMetadata where

import           Data.Aeson (ToJSON, FromJSON)
import           Data.ByteString (ByteString)
import           Data.Text (Text)

import           GHC.Generics

data UserMetadata = UserMetadata
  { phone_number :: Maybe Text
  } deriving (Eq, Show, Generic)
instance ToJSON UserMetadata
instance FromJSON UserMetadata

 
