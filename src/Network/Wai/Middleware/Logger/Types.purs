module Network.Wai.Middleware.Logger.Types where

import Effect (Effect)
import Network.Wai (Request, Response)

type Formatter (sym :: Symbol) = (Request -> Response -> ApplicationTime -> Effect String)

newtype Token = Token (Request -> Response -> ApplicationTime -> String)

type ApplicationTime = 
  { -- | Time between request received and application finished processing it
    process :: Number
  , -- | Time between request received and response sent
    full    :: Number
  }

token :: (Request -> Response -> ApplicationTime -> String) -> Token 
token = Token 