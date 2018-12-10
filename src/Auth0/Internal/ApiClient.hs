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

-- * Authentication API
-- -- * Authorization API
doPostClientToken :: ClientCredentialsRequest -> ClientM ClientToken
-- doAuthorize :: Text -> Text -> Text -> Text -> ClientM 
-- -- * Authorization Extension API
doGetAllGroups :: Maybe Token -> ClientM Groups
doGetAllGroupMembers :: Maybe Token -> Text -> ClientM GroupMembers
doGetAllRoles :: Maybe Token -> ClientM Roles
doPatchUserRoles :: Maybe Token -> Text -> PatchUserRolesBody -> ClientM ()
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
  ( doPostClientToken
  -- -- * Authorization Extension API
  :<|> 
    ( doGetAllGroups
    :<|> doGetAllGroupMembers
    :<|> doGetAllRoles
    :<|> doPatchUserRoles
    )
  -- -- * UserProfile API 
  :<|> doGetUserinfo 
  ) 
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
requestClientCredentialsToken :: ConnectionInfo -> Auth0ApiResponse ClientToken
requestClientCredentialsToken connInfo = do 
  requestClientToken connInfo mkClientCredentialsRequest

requestAuthorizationToken :: ConnectionInfo -> Text -> Auth0ApiResponse ClientToken
requestAuthorizationToken connInfo redirectUrl = do 
  requestClientToken connInfo (mkAuthorizationCodeRequest redirectUrl)

requestClientToken :: 
  ConnectionInfo 
  -> (Text -> Text -> Text -> ClientCredentialsRequest) -- ^ client_id -> client_secret -> audience. grant_type has been set  
  -> Auth0ApiResponse ClientToken
requestClientToken ConnectionInfo{..} clientCredentialsRequest = do
  manager' <- newManager tlsManagerSettings
  let clientCreds = clientCredentialsRequest (Enc8.decodeUtf8 cClientId) (Enc8.decodeUtf8 cClientSecret) ("https://" <> cDomain <> "/api/v2/")
  runClientM (doPostClientToken clientCreds) (ClientEnv manager' (BaseUrl Https (T.unpack cDomain) 443 ""))

-- -- * Authentication > Authorization Extension API

getAllGroups :: 
  ConnectionInfo
  -> AccessToken
  -> Auth0ApiResponse Groups
getAllGroups conn token = 
  withToken conn token (\mToken -> doGetAllGroups mToken)

getAllGroupMembers :: 
  ConnectionInfo
  -> AccessToken
  -> Text
  -> Auth0ApiResponse GroupMembers
getAllGroupMembers conn token groupId = 
  withToken conn token (\mToken -> doGetAllGroupMembers mToken groupId)

getAllRoles :: 
  ConnectionInfo
  -> AccessToken
  -> Auth0ApiResponse Roles
getAllRoles conn token = 
  withToken conn token (\mToken -> doGetAllRoles mToken)

addRoles :: 
  ConnectionInfo -- ^ See https://auth0.com/docs/api/authorization-extension#find-your-extension-url
                 --   Ex. https://xyz.eu.webtask.io/adf6e...01/api
  -> AccessToken
  -> Text -- ^ User Id
  -> PatchUserRolesBody -- ^ [ "{role_id}" ]
  -> Auth0ApiResponse ()
addRoles conn token userId body = 
  withToken conn token (\mToken -> doPatchUserRoles mToken userId body)

-- -- * Authentication > Profile API
getUserInfo :: Text -> AccessToken -> Auth0ApiResponse UserInfo
getUserInfo domain token =  
  withToken (defaultConnectionInfo domain) token (\mToken -> doGetUserinfo mToken)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- Helper
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

withToken :: ConnectionInfo -> AccessToken -> (Maybe Token -> ClientM a) -> Auth0ApiResponse a
withToken ConnectionInfo{..} token f = do
  manager' <- newManager tlsManagerSettings
  let authToken = Just $ mkToken token
  runClientM (f authToken) (ClientEnv manager' (BaseUrl cScheme (T.unpack cDomain) cPort (T.unpack cPath)))
