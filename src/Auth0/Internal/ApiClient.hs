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
doPostClientToken :: ClientCredentialsRequest -> ClientM ClientToken
-- -- * UserProfile API
doGetUserinfo :: Maybe Token -> ClientM UserInfo
-- * Management API
-- -- * Connections API
doGetConnections :: Maybe Token -> ClientM [Connection]
-- -- * Users API
doGetUsers :: Maybe Token -> ClientM [User]
doGetUser :: Maybe Token -> Text -> ClientM User
doPostUser :: Maybe Token -> PostUserBody -> ClientM User
doPatchUser :: Maybe Token -> Text -> PatchUserBody -> ClientM User
doDeleteUser :: Maybe Token -> Text -> ClientM ()
( 
  -- -- * Authorization API
  ( doPostClientToken :<|> doGetUserinfo ) 
  :<|> 
  -- -- * Management API
  ( 
   ( doGetConnections :<|> doGetUsers :<|> doGetUser :<|> doPostUser :<|> doPatchUser :<|> doDeleteUser )
  ) 
 ) = client api

type AccessToken = BS.ByteString
type Auth0ApiResponse a = IO (Either ServantError a)

-- * Management API
-- -- * Management > Connections API
getConnections :: Text -> AccessToken -> Auth0ApiResponse [Connection]
getConnections domain token = withToken (defaultConnectionInfo domain) token doGetConnections 

-- -- * Management > Users API
getUsers :: Text -> AccessToken -> Auth0ApiResponse [User]
getUsers domain token =
  withToken (defaultConnectionInfo domain) token doGetUsers

getUser :: Text -> AccessToken -> Text -> Auth0ApiResponse User
getUser domain token userId = 
  withToken (defaultConnectionInfo domain) token (\mToken -> doGetUser mToken userId)

createUser :: Text -> AccessToken -> PostUserBody -> Auth0ApiResponse User
createUser domain token body =
  withToken (defaultConnectionInfo domain) token (\mToken -> doPostUser mToken body)

updateUser :: Text -> AccessToken -> Text -> PatchUserBody -> Auth0ApiResponse User
updateUser domain token userId body =
  withToken (defaultConnectionInfo domain) token (\mToken -> doPatchUser mToken userId body)

deleteUser :: Text -> AccessToken -> Text -> Auth0ApiResponse ()
deleteUser domain token userId = 
  withToken (defaultConnectionInfo domain) token (\mToken -> doDeleteUser mToken userId)

-- * Authentication API
-- -- * Authentication > Authorization API

-- | Manage permissions through: 
-- | Dashboard > API > Auth0 Management API > Machine to Machine Applications
requestClientToken :: ConnectionInfo -> Auth0ApiResponse ClientToken
requestClientToken ConnectionInfo{..} = do 
  manager' <- newManager tlsManagerSettings
  let clientCreds = defaultClientCredentialsRequest (Enc8.decodeUtf8 cClientId) (Enc8.decodeUtf8 cClientSecret) ("https://" <> cDomain <> "/api/v2/")
  runClientM (doPostClientToken clientCreds) (ClientEnv manager' (BaseUrl Https (T.unpack cDomain) 443 ""))

-- -- * Authentication > Profile API
getUserInfo :: Text -> AccessToken -> IO (Either ServantError UserInfo)
getUserInfo domain token =  
  withToken (defaultConnectionInfo domain) token (\mToken -> doGetUserinfo mToken)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- Helper
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

withToken :: ConnectionInfo -> AccessToken -> (Maybe Token -> ClientM a) -> Auth0ApiResponse a
withToken ConnectionInfo{..} token f = do
  manager' <- newManager tlsManagerSettings
  let authToken = Just $ mkToken token
  runClientM (f authToken) (ClientEnv manager' (BaseUrl cScheme (T.unpack cDomain) cPort ""))
