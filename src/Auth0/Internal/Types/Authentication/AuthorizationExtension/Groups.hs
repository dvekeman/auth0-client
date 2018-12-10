{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Authentication.AuthorizationExtension.Groups where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text ( Text )

import           GHC.Generics

data Groups = Groups 
  { groups :: [Group]
  , total :: Int
  } deriving (Eq, Show, Generic)
instance ToJSON Groups where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON Groups where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

data Group = Group
  { _id             :: Text
  , name            :: Text
  , description     :: Text
  , members         :: [Text]
  , roles           :: [Text]
  , mappings        :: [Text]
  } deriving (Eq, Show, Generic)

instance ToJSON Group where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON Group where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

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
  , name          :: Text
  , user_id       :: Text
  , nickname      :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON GroupMember where 
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }
instance FromJSON GroupMember where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }
