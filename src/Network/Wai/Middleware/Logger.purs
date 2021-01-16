module Network.Wai.Middleware.Logger where

import Prelude

import Data.JSDate as JSDate
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Number.Format as Number
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Network.HTTP.Types (hContentLength)
import Network.Wai (Middleware,Response(..))
import Network.Wai.Internal (ResponseReceived(..))
import Network.Wai.Middelware.Logger.Internal (formatLineToken)
import Network.Wai.Middleware.Logger.Types (Token, Formatter, token)
import Node.Encoding (Encoding(..))
import Node.Stream (Writable)
import Node.Stream as Stream

-- | Accepts a formatter which is just a function that will use a specific format to log application
-- | Writable is just a stream where the log will be pushed to. stdout from Node.Process is a commong one.
loggerMiddleware :: forall sym. Formatter sym -> Writable () -> Middleware 
loggerMiddleware formatter stream app req res = do 
  tstart  <- liftEffect hrtime
  app req \resp -> do 
    tprocess <- liftEffect $ hrtime' tstart
    _ <- res resp 
    tfull <- liftEffect $ hrtime' tstart
    let  responseTime = { process: hrtimeDiffMs tprocess, full: hrtimeDiffMs tfull }
    line <- liftEffect $ formatter req resp responseTime
    liftEffect $ void $ Stream.writeString stream UTF8 (line <> " \n") (pure unit) 
    pure ResponseReceived

hrtimeDiffMs :: Tuple Seconds Nanoseconds -> Number
hrtimeDiffMs (Tuple sec nano) = sec * 1000000000.00 + nano / 1000000.00

type ApacheCombined = "{remoteHost} - {remoteUser} {date} \"{method} {url} {httpVersion}\" {status} {contentLength} \"{referer}\" \"{userAgent}\""

apacheCombined :: Formatter ApacheCombined 
apacheCombined req res time = do 
  date       <- (JSDate.toUTCString) <$> JSDate.now
  pure $ formatLineToken req res time (SProxy :: _ ApacheCombined)
    { date: token \rq rs t -> date
    , remoteHost
    , remoteUser
    , method
    , url
    , httpVersion
    , referer 
    , userAgent 
    , contentLength
    , status 
    }

type Dev = "\x1b[0m{method} {url} \x1b[{color}m{status}\x1b[0m {responseTime} ms - {contentLength}\x1b[0m"

dev :: Formatter Dev 
dev req res time = do
  pure $ formatLineToken req res time (SProxy :: _ Dev) 
    { responseTime
    , color
    , method
    , url
    , status
    , contentLength
    }
  where 
    responseTime = token \rq rs t -> Number.toStringWith (Number.fixed 3) time.full 
    color = token \rq rs t -> show $ toColor $ fromMaybe 0 $ status' res
    toColor st
      | st >= 500 = 31 -- red
      | st >= 400 = 33 -- yellow 
      | st >= 300 = 36 -- cyan 
      | st >= 200 = 32 -- green 
      | otherwise = 0  -- no color 

remoteUser :: Token
remoteUser =  token \_ _ _ -> ""

remoteHost :: Token
remoteHost = token \req _ _ -> fromMaybe "-" $ _.remoteHost $ unwrap req

method :: Token
method = token \req _ _ -> show $ _.method $ unwrap req 

url :: Token  
url = token \req _ _ -> _.url $ unwrap req 

httpVersion :: Token 
httpVersion = token \req _ _ -> show $ _.httpVersion $ unwrap req

referer :: Token
referer = token \req _ _ -> fromMaybe "-" $ _.referer $ unwrap req 

userAgent :: Token
userAgent = token \req _ _ -> fromMaybe "-" $ _.userAgent $ unwrap req

status :: Token
status = token \_ res _ -> case status' res of 
  Just code -> show code
  _         -> mempty

status' :: Response -> Maybe Int 
status' = case _ of 
  ResponseString st _ _ -> Just st.code
  _                     -> Nothing

contentLength :: Token
contentLength = token \_ res _ -> case res of 
  ResponseString _ hdrs _ -> fromMaybe mempty $ Map.lookup hContentLength $ Map.fromFoldable hdrs
  _                       -> mempty 

hrtime :: Effect (Tuple Seconds Nanoseconds)
hrtime = hrtimeImpl Tuple 

hrtime' :: Tuple Seconds Nanoseconds -> Effect (Tuple Seconds Nanoseconds)
hrtime' (Tuple sec nano) = hrtimeImpl_ [sec, nano] Tuple

type Nanoseconds =  Number 
type Seconds =  Number 

foreign import  hrtimeImpl  ::  (Seconds -> Nanoseconds -> (Tuple Seconds Nanoseconds)) -> Effect (Tuple Seconds Nanoseconds)
foreign import  hrtimeImpl_ :: Array Number -> (Seconds -> Nanoseconds -> (Tuple Seconds Nanoseconds)) -> Effect (Tuple Seconds Nanoseconds)