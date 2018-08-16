module Auth0.Internal.ApiClient where

import qualified Data.ByteString as BS
import           Data.Monoid ( (<>) )
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc8
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client (runClientM, ClientEnv(..), ServantError, Scheme(..), BaseUrl(..))

import           Auth0.Internal.Api
import qualified Auth0.Internal.Types.Management.ClientToken as CT

-- * Authentication API
-- -- * Authorization API
doRequestClientToken :: ClientCredentialsRequest -> ClientM ClientToken
-- -- * UserProfile API
doGetUserinfo :: Maybe Token -> ClientM UserInfo
-- * Management API
-- -- * Connections API
doGetConnections :: Maybe Token -> ClientM [Connection]
-- -- * Users API
doGetUsers :: Maybe Token -> ClientM [User]
doPostUser :: Maybe Token -> PostUserBody -> ClientM User
-- -- * Authorization API
( doRequestClientToken :<|> doGetUserinfo ) :<|> ( doGetConnections :<|> doGetUsers :<|> doPostUser ) = client api

type AccessToken = BS.ByteString
type Auth0ApiResponse a = IO (Either ServantError a)

-- * Management API
-- -- * Management > Connections API
getConnections :: Text -> AccessToken -> Auth0ApiResponse [Connection]
getConnections domain token = withToken (defaultConnectionInfo domain) token doGetConnections 

-- -- * Management > Users API
getUsers :: AccessToken -> Auth0ApiResponse [User]
getUsers token = do
  manager' <- newManager tlsManagerSettings
  let authToken = Just $ mkToken token
  runClientM (doGetUsers authToken) (ClientEnv manager' (BaseUrl Https "hadruki.eu.auth0.com" 443 ""))

postUser :: AccessToken -> PostUserBody -> Auth0ApiResponse User
postUser token body = do
  manager' <- newManager tlsManagerSettings
  let authToken = Just $ mkToken token
  runClientM (doPostUser authToken body) (ClientEnv manager' (BaseUrl Https "hadruki.eu.auth0.com" 443 ""))

-- * Authentication API
-- -- * Authentication > Authorization API

-- | Manage permissions through: 
-- | Dashboard > API > Auth0 Management API > Machine to Machine Applications
requestClientToken :: ConnectionInfo -> Auth0ApiResponse ClientToken
requestClientToken ConnectionInfo{..} = do 
  manager' <- newManager tlsManagerSettings
  let clientCreds = defaultClientCredentialsRequest (Enc8.decodeUtf8 cClientId) (Enc8.decodeUtf8 cClientSecret) ("https://" <> cDomain <> "/api/v2/")
  runClientM (doRequestClientToken clientCreds) (ClientEnv manager' (BaseUrl Https (T.unpack cDomain) 443 ""))

-- -- * Authentication > Profile API
getUserInfo :: ClientToken -> IO (Either ServantError UserInfo)
getUserInfo CT.ClientToken{..} = getUserInfo' (Enc8.encodeUtf8 access_token)

getUserInfo' :: BS.ByteString -> IO (Either ServantError UserInfo)
getUserInfo' token = do
  manager' <- newManager tlsManagerSettings
  let authToken = Just $ mkToken token
  runClientM (doGetUserinfo authToken) (ClientEnv manager' (BaseUrl Https "hadruki.eu.auth0.com" 443 ""))


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- Helper
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

withToken :: ConnectionInfo -> AccessToken -> (Maybe Token -> ClientM a) -> Auth0ApiResponse a
withToken ConnectionInfo{..} token f = do
  manager' <- newManager tlsManagerSettings
  let authToken = Just $ mkToken token
  runClientM (f authToken) (ClientEnv manager' (BaseUrl cScheme (T.unpack cDomain) cPort ""))
