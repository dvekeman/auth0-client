{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Authentication.AuthorizationExtension.Roles where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text ( Text )

import           GHC.Generics

data Roles = Roles 
  { roles :: [Role]
  , total :: Int
  } deriving (Eq, Show, Generic)
instance ToJSON Roles where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON Roles where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

data Role = Role
  { applicationType :: Text
  , applicationId   :: Text
  , description     :: Text
  , permissions     :: [Text]
  , _id             :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Role where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON Role where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }
