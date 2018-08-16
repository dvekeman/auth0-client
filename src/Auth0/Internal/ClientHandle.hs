module Auth0.Internal.ClientHandle where

import           Auth0.Internal.Types

data ClientHandle = ClientHandle
  { hConnectInfo :: ConnectionInfo
  }
