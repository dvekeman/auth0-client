{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Authentication.AuthorizationExtension.GroupMembers where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text ( Text )

import           GHC.Generics

data GroupMembers = GroupMembers 
  { users :: [GroupMember]
  , total :: Int
  } deriving (Eq, Show, Generic)
instance ToJSON GroupMembers where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON GroupMembers where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

data GroupMember = GroupMember
  { email         :: Text
  , username      :: Text
  , name          :: Maybe Text
  , user_id       :: Text
  , nickname      :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON GroupMember where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON GroupMember where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }
