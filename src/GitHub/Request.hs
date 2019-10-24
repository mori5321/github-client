{-# LANGUAGE OverloadedStrings #-}

module GitHub.Request 
    ( Request(..)
    , execRequest
    , getResponseBody
    , Method(..)
    , Path
    )
where

import Data.Aeson (Value, FromJSON, ToJSON)
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Char8 as S8

import Control.Monad.IO.Class (MonadIO)
import Control.Applicative (pure)

import Network.HTTP.Simple ( setRequestPath
                           , setRequestHost
                           , setRequestHeader
                           , setRequestMethod
                           , setRequestPort
                           , setRequestSecure
                           , setRequestManager
                           , defaultRequest
                           , httpJSON
                           , httpLBS
                           , httpJSONEither
                           , getResponseStatusCode
                           , getResponseHeader
                           , getResponseBody
                           )
import qualified Network.HTTP.Simple as HTTP

import GitHub.Auth (setRequestAuth, Auth(..))


data Method = GET | POST | PATCH | DELETE deriving Show

type Path = S8.ByteString
-- data Request = Request { path :: Path
--                        , method :: Method
--                        }


data Request = Request { path :: Path, method :: Method}
    

-- TODO
-- クラスをつくる
-- Response Valueとして使えるクラス
-- = FromJSON, ToJSONできて
-- = Show できるやつ

execRequest :: (FromJSON r) => Request -> Auth -> IO (HTTP.Response r)
execRequest auth = sendRequest . mkHttpRequest auth


mkHttpRequest :: Request -> Auth -> HTTP.Request
mkHttpRequest req auth = 
    setRequestPath (path req)
    $ setRequestMethod (S8.pack . show $ method req)
    $ setRequestHeader "Content-Type" ["application/json"]
    $ setRequestAuth auth
    $ setRequestHeader "User-Agent" ["Github-API-Client"]
    $ setRequestPort 443
    $ setRequestSecure True 
    $ setRequestHost "api.github.com"
    $ defaultRequest

-- TODO: あとでいい感じな型定義にしたい
-- sendRequest :: (FromJSON b, MonadIO m) => Request -> m (Response b)
-- sendRequest :: (FromJSON b, Show b) => Request -> IO (Response b)
sendRequest :: (FromJSON r) => HTTP.Request -> IO (HTTP.Response r)
sendRequest = httpJSON
