{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Auth0.Internal.Api
  ( module Auth0.Internal.Types
  , module Servant.API
  , module Servant.Client
  , Auth0Api
  , api
  ) where

import           Data.Aeson
import           Data.Proxy
import           Servant.API
import           Servant.Client

import           Auth0.Internal.AuthenticationApi
import           Auth0.Internal.ManagementApi
import           Auth0.Internal.Types

type Auth0Api = 
  AuthenticationApi
  :<|> ManagementApi

api :: Proxy Auth0Api
api = Proxy

