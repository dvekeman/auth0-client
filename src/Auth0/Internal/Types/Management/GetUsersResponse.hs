{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Management.GetUsersResponse where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import Auth0.Internal.Types.Management.User

import           GHC.Generics

data GetUsersResponse = GetUsersResponse
  { users :: [User]
  , total :: Int
  , start :: Int
  , limit :: Int
  , length :: Int
  } deriving (Eq, Show, Generic)
instance ToJSON GetUsersResponse where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON GetUsersResponse where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }