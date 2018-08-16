{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth0.Internal.AuthenticationApi where

import           Servant.API

import           Auth0.Internal.Types

type AuthenticationApi = 
  AuthorizationApi
  :<|> UserProfileApi

type AuthorizationApi = 
  -- POST /oauth/token 
  "oauth" :> "token" :> ReqBody '[JSON] ClientCredentialsRequest :> Post '[JSON] ClientToken

type UserProfileApi = 
  Header "Authorization" Token :> "userinfo" :> Get '[JSON] UserInfo

