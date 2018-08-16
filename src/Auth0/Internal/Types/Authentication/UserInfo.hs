{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Authentication.UserInfo where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text ( Text )

import           GHC.Generics

data UserInfo = UserInfo
  { email_verified :: Maybe Bool
  , email          :: Maybe Text
  , updated_at     :: Text
  , name           :: Text
  , picture        :: Text
  , user_id        :: Maybe Text
  , nickname       :: Text
  , created_at     :: Maybe Text
  , sub            :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON UserInfo where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON UserInfo where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }
