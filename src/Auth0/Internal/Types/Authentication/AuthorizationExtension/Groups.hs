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

