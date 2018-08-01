{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Auth0.Internal.Api where

import           Data.Aeson
import           Data.ByteString ( ByteString )
import           Data.Monoid     ( (<>) )
import           Data.Text       ( Text )
import qualified Data.Text.Encoding as Enc8
import           Data.Proxy
import           Servant.API
import           Servant.Client

import           Auth0.Internal.Types.UserInfo

import           GHC.Generics

type Token = Text 

mkToken :: ByteString -> Token
mkToken token = "Bearer " <> Enc8.decodeUtf8 token

type Auth0Api = UserProfileApi

type UserProfileApi = 
  Header "Authorization" Token :> "userinfo" :> Get '[JSON] UserInfo

api :: Proxy Auth0Api
api = Proxy

userinfo :: Maybe Token -> ClientM UserInfo
userinfo = client api

