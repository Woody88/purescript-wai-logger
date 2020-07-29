module Network.Wai.Middleware.Logger.Types where

import Effect (Effect)
import Network.Wai (class WaiRequest, Response)

type Formatter (sym :: Symbol) = (forall req. WaiRequest req => req -> Response -> ApplicationTime -> Effect String)

newtype Token = Token (forall req. WaiRequest req => req -> Response -> ApplicationTime -> String)

type ApplicationTime = 
  { -- | Time between request received and application finished processing it
    process :: Number
  , -- | Time between request received and response sent
    full    :: Number
  }

token :: (forall req. WaiRequest req => req -> Response -> ApplicationTime -> String) -> Token 
token = Token 