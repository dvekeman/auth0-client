{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.UserIdentity where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text (Text)

import           GHC.Generics

data UserIdentity = UserIdentity
  { user_id :: Maybe Text 
  , provider :: Maybe Text
  , connection :: Maybe Text
  , isSocial :: Maybe Bool
  } deriving (Eq, Show, Generic)
instance ToJSON UserIdentity where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON UserIdentity where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

