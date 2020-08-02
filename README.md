# wai-logger

Wai logging middleware. Currently only provides Apache combined & Dev log ouput.

## Installation

***This library is not yet published to pursuit.***  
You can install this package by adding it to your packages.dhall:

```dhall
let additions =
    {   wai =
            { dependencies =
                [ "http-types"
                , "node-buffer"
                , "node-http"
                , "node-net"
                , "node-streams"
                , "node-url"
                ]
            , repo =
                "https://github.com/Woody88/purescript-wai.git"
            , version =
                "master"
            }
    ,   wai-logger =
            { dependencies =
                [ "console"
                , "effect"
                , "node-process"
                , "numbers"
                , "record-format"
                , "wai"
                ]
            , repo =
                "https://github.com/Woody88/purescript-wai-logger.git"
            , version =
                "master"
            }         
    ,   http-types =
            { dependencies =
                [ "console"
                , "effect"
                , "psci-support"
                , "tuples"
                , "unicode"
                , "uri"
                ]
            , repo =
                "https://github.com/Woody88/purescript-http-types.git"
            , version =
                "master"
            }
    }
```
```console
user@user:~$ spago install wai-logger
```

## Usage 

### Hello World (Using Warp)
```purescript 
import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console as Console
import Network.HTTP.Types (ok200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, responseStr)
import Network.Wai.Middleware.Logger (apacheCombined, loggerMiddleware)
import Network.Warp.Run (runSettings)
import Network.Warp.Settings (defaultSettings)
import Node.Process (stdout)

main :: Effect Unit
main = do 
    let beforeMainLoop = Console.log $ "Listening on port " <> show defaultSettings.port
    void $ runSettings defaultSettings { beforeMainLoop = beforeMainLoop }
         $ loggerMiddleware apacheCombined stdout 
         $ app
         
app :: Application 
app req f = do
    f $ responseStr ok200 [(hContentType /\ "text/plain")] "Hello, World!"
```