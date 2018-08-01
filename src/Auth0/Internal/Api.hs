{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Auth0.Internal.Api where

import           Data.Aeson
import           Data.ByteString ( ByteString )
import           Data.Proxy
import           Servant.API
import           Servant.Client

import           Auth0.Internal.Types.UserInfo

import           GHC.Generics

type Token = ByteString 

instance ToHttpApiData ByteString

mkToken :: ByteString -> Token
mkToken tok = tok

type Auth0Api = UserProfileApi

type UserProfileApi = 
  Header "Authorization" Token :> "userinfo" :> Get '[JSON] UserInfo

api :: Proxy Auth0Api
api = Proxy

userinfo :: Maybe Token -> ClientM UserInfo
userinfo = client api

