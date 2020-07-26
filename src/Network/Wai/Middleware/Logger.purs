module Network.Wai.Middleware.Logger where

import Prelude

import Data.JSDate as JSDate
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format as Number
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Network.HTTP.Types (hContentLength)
import Network.HTTP.Types as H
import Network.Wai (class WaiRequest, Middleware, Response(..))
import Network.Wai as Wai
import Node.Encoding (Encoding(..))
import Node.Stream (Writable)
import Node.Stream as Stream
import Record.Format as Record

type FormatFunc = (forall req. WaiRequest req => req -> Response -> ApplicationTime -> Effect String)

type Nanoseconds =  Number 
type Seconds =  Number 

type ApplicationTime = 
  { -- | Time between request received and application finished processing it
    process :: Number
  , -- | Time between request received and response sent
    full    :: Number
  }

-- | Accepts a format which is just a function that will use a specific format to log application
-- | Writable is just a stream where the log will be pushed to. stdout from Node.Process is a commong one.
loggerMiddleware :: FormatFunc -> Writable () -> Middleware 
loggerMiddleware format stream app req res = do 
  tstart  <- liftEffect hrtime
  app req \resp -> do 
    tprocess <- liftEffect $ hrtime' tstart
    _ <- res resp 
    tfull <- liftEffect $ hrtime' tstart
    let  responseTime = { process: hrtimeDiffMs tprocess, full: hrtimeDiffMs tfull }
    line <- liftEffect $ format req resp responseTime
    liftEffect $ void $ Stream.writeString stream UTF8 (line <> " \n") (pure unit) 

hrtimeDiffMs :: Tuple Seconds Nanoseconds -> Number
hrtimeDiffMs (Tuple sec nano) = sec * 1000000000.00 + nano / 1000000.00

combined :: FormatFunc 
combined req res _ = do 
  remoteHost <- fromMaybe "-" <$> Wai.remoteHost req
  date       <- (JSDate.toUTCString) <$> JSDate.now
  pure $ Record.format (SProxy :: _ "{remoteHost} - {remoteUser} {date} \"{method} {url} {httpVersion}\" {status} {contentLength} \"{referer}\" \"{userAgent}\"") 
    { remoteHost
    , remoteUser
    , date
    , method: method req 
    , url: url req 
    , httpVersion: httpVersion req 
    , referer: referer req 
    , userAgent: userAgent req 
    , contentLength: contentLength res 
    , status: status res 
    }
    
dev :: FormatFunc 
dev req res time = do
  pure $ Record.format (SProxy :: _ "\x1b[0m{method} {url} \x1b[{color}m{status}\x1b[0m {responseTime} ms - {contentLength}\x1b[0m") 
    { method: method req 
    , url: url req 
    , responseTime: Number.toStringWith (Number.fixed 3) time.full 
    , status: status res 
    , color: color $ status res
    , contentLength: contentLength res 
    }
  where 
    color st
      | st >= 500 = 31 -- red
      | st >= 400 = 33 -- yellow 
      | st >= 300 = 36 -- cyan 
      | st >= 200 = 32 -- green 
      | otherwise     = 0  -- no color 

remoteUser :: String
remoteUser =  "" 

method :: ∀ hdl. WaiRequest hdl ⇒ hdl → H.Method
method = Wai.method 

url :: ∀ hdl. WaiRequest hdl ⇒ hdl → String 
url = Wai.url   

httpVersion :: ∀ hdl. WaiRequest hdl ⇒ hdl → H.HttpVersion
httpVersion = Wai.httpVersion

referer :: ∀ hdl. WaiRequest hdl ⇒ hdl → String
referer = fromMaybe ""  <<< Wai.referer  

userAgent :: ∀ hdl. WaiRequest hdl ⇒ hdl → String 
userAgent = Wai.userAgent  

status :: Response -> Int
status res = fromMaybe (-1) $ case res of 
  ResponseString st _ _ -> Just st.code
  _                     -> Nothing

contentLength :: Response -> String 
contentLength res = fromMaybe "" case res of 
  ResponseString _ hdrs _ -> Map.lookup hContentLength $ Map.fromFoldable hdrs
  _                     -> Nothing

hrtime :: Effect (Tuple Seconds Nanoseconds)
hrtime = hrtimeImpl Tuple 

hrtime' :: Tuple Seconds Nanoseconds -> Effect (Tuple Seconds Nanoseconds)
hrtime' (Tuple sec nano) = hrtimeImpl_ [sec, nano] Tuple

foreign import  hrtimeImpl  ::  (Seconds -> Nanoseconds -> (Tuple Seconds Nanoseconds)) -> Effect (Tuple Seconds Nanoseconds)
foreign import  hrtimeImpl_ :: Array Number -> (Seconds -> Nanoseconds -> (Tuple Seconds Nanoseconds)) -> Effect (Tuple Seconds Nanoseconds)