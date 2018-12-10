{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth0.Internal.AuthenticationApi where

import           Data.Text (Text)

import           Servant.API

import           Auth0.Internal.Types

type AuthenticationApi = 
  AuthorizationApi
  :<|> AuthorizationExtensionApi
  :<|> UserProfileApi

type AuthorizationApi = 
  -- POST /oauth/token 
  "oauth" :> "token" :> ReqBody '[JSON] ClientCredentialsRequest :> Post '[JSON] ClientToken
--   :<|> "authorize" 
--           :> QueryParam "audience" Text 
--           :> QueryParam "scope" Text 
--           :> QueryParam "response_type" Text 
--           :> QueryParam "client_id" Text 

type AuthorizationExtensionApi = 
  -- GET /groups
  Header "Authorization" Token :> "groups" :> Get '[JSON] Groups
  -- GET /groups/<group_id>/members
  :<|> Header "Authorization" Token :> "groups" :> Capture "group_id" Text :> "members" :> Get '[JSON] GroupMembers
  -- GET /users/roles
  :<|> Header "Authorization" Token :> "roles" :> Get '[JSON] Roles
  -- PATCH /users/{user_id}/roles
  :<|> Header "Authorization" Token :> "users" :> Capture "user_id" Text :> "roles" :> ReqBody '[JSON] PatchUserRolesBody :> Patch '[JSON] () 

type UserProfileApi = 
  Header "Authorization" Token :> "userinfo" :> Get '[JSON] UserInfo

