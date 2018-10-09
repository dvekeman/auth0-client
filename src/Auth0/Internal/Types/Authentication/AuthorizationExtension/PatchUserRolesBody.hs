{-# LANGUAGE DeriveGeneric #-}

module Auth0.Internal.Types.Authentication.AuthorizationExtension.PatchUserRolesBody where

import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, genericToJSON, genericParseJSON, defaultOptions)
import           Data.Aeson.Types (omitNothingFields)
import           Data.Text (Text)

import           GHC.Generics

type PatchUserRolesBody = [ Text ]
