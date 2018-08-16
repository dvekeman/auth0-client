{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth0.Internal.ManagementApi where

import           Data.Text (Text)
import           Servant.API

import           Auth0.Internal.Types

type ManagementApi = 
  ConnectionApi 
  :<|> UsersApi

type ConnectionApi = 
  -- GET /api/v2/connections
  Header "Authorization" Token :> "api" :> "v2" :> "connections" :> Get '[JSON] [Connection]

type UsersApi = 
  -- GET /api/v2/users List or search users
  Header "Authorization" Token :> "api" :> "v2" :> "users" :> Get '[JSON] [User] 
  -- POST /api/v2/users Create a user
  :<|> Header "Authorization" Token :> "api" :> "v2" :> "users" :> ReqBody '[JSON] PostUserBody :> Post '[JSON] User   
  -- PATCH /api/v2/users/{id} Update a user
  :<|> Header "Authorization" Token :> "api" :> "v2" :> "users" :> Capture "id" Text :> ReqBody '[JSON] PatchUserBody :> Patch '[JSON] User
  -- DELETE /api/v2/users/{id} Delete a user
  :<|> Header "Authorization" Token :> "api" :> "v2" :> "users" :> Capture "id" Text :> Delete '[JSON] ()