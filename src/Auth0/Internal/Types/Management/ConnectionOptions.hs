{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.ConnectionOptions where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text (Text)

import           GHC.Generics

data ConnectionOptions = ConnectionOptions
  { mfa :: Maybe MultiFactorAuthentication
  , passwordPolicy :: Maybe Text
  , brute_force_protection :: Maybe Bool
  , strategy_version :: Maybe Int
  , requires_username :: Maybe Bool
  , validation :: Maybe Validation
  , email :: Maybe Bool
  , profile :: Maybe Bool
  , scope :: Maybe [Text]
  } deriving (Eq, Show, Generic)
instance ToJSON ConnectionOptions where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON ConnectionOptions where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

data MultiFactorAuthentication = MultiFactorAuthentication
  { active :: Bool
  , return_enroll_settings :: Bool
  } deriving (Eq, Show, Generic)
instance ToJSON MultiFactorAuthentication where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON MultiFactorAuthentication where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

data Validation = Validation 
  { username :: Maybe UsernameValidation
  } deriving (Eq, Show, Generic)
instance ToJSON Validation where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON Validation where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

data UsernameValidation = UsernameValidation
  { min :: Int
  , max :: Int
  } deriving (Eq, Show, Generic)
instance ToJSON UsernameValidation where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON UsernameValidation where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }
