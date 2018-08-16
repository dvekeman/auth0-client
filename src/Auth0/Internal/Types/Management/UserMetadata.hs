{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.UserMetadata where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text (Text)

import           GHC.Generics

data UserMetadata = UserMetadata
  { phone_number :: Maybe Text
  } deriving (Eq, Show, Generic)
instance ToJSON UserMetadata where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON UserMetadata where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

 
