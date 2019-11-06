{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module GitHub.Request 
    ( IsRequest(..)
    , IsHTTPQueryItem(..)
    , Method(..)
    , Request(..)
    , Path
    , getResponseBody
    , getResponseStatusCode
    , sendRequest
    , sendRequest'
    , mkHttpRequestDefault
    , withAuth
    , withQuery
    , withBody
    )
where

import GHC.Generics
import Data.Aeson ( Value, FromJSON, ToJSON )
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8

import Network.HTTP.Simple ( setRequestPath
                           , setRequestHost
                           , setRequestHeader
                           , setRequestBodyJSON
                           , setRequestMethod
                           , setRequestPort
                           , setRequestSecure
                           , setRequestManager
                           , setRequestQueryString
                           , defaultRequest
                           , httpJSON
                           , httpJSONEither
                           , JSONException
                           , getResponseStatusCode
                           , getResponseHeader
                           , getResponseBody
                           )
import qualified Network.HTTP.Simple as HTTP

import GitHub.Auth ( setRequestAuth, Auth(..) )

data Method = GET | POST | PATCH | DELETE deriving Show

type Path = T.Text

data Request = Request { reqPath :: Path
                       , reqMethod :: Method
                       } deriving (Show, Generic)

class IsHTTPQueryItem a where
    toHTTPQueryItem :: a -> HTTP.QueryItem

class IsRequest request where
    path :: request -> Path
    method :: request -> Method
    mkHttpRequest :: request -> HTTP.Request
    mkHttpRequest = mkHttpRequestDefault

instance IsRequest Request where
    path = reqPath
    method = reqMethod
    
withAuth :: Auth -> HTTP.Request -> HTTP.Request
withAuth = setRequestAuth

withBody :: ToJSON b => b -> HTTP.Request -> HTTP.Request
withBody = setRequestBodyJSON

withQuery :: HTTP.Query -> HTTP.Request -> HTTP.Request
withQuery = setRequestQueryString

mkHttpRequestDefault :: IsRequest r => r -> HTTP.Request
mkHttpRequestDefault req = 
    setRequestPath (S8.pack (T.unpack (path req)))
        $ setRequestMethod (S8.pack . show $ method req)
        $ setDefaultHeaders
        $ setDefaultConfigs
        $ defaultRequest

-- execRequest :: (FromJSON b) => HTTP.Request -> IO b
-- execRequest req = getResponseBody <$> sendRequest req

-- execRequest :: (IsRequest req, ToJSON reqBody, FromJSON resBody) => req -> reqBody -> Auth -> IO (HTTP.Response resBody)
-- execRequest req body auth = sendRequest (mkHttpRequest req body auth)

-- execRequest :: (IsRequest req, ToJSON reqBody, FromJSON resBody) => req -> reqBody -> Auth -> IO (HTTP.Response)
-- execRequest req body auth = do
--     response <- sendRequest (mkHttpRequest req body auth)
--     let body = getResponseBody response
--     return body


setDefaultHeaders :: HTTP.Request -> HTTP.Request
setDefaultHeaders = 
    setRequestHeader "Content-Type" ["application/json"]
    . setRequestHeader "User-Agent" ["Github-API-Client"]

setDefaultConfigs :: HTTP.Request -> HTTP.Request
setDefaultConfigs =
    setRequestPort 443
    . setRequestSecure True 
    . setRequestHost "api.github.com"

sendRequest :: (FromJSON r) => HTTP.Request -> IO (HTTP.Response r)
sendRequest = httpJSON

sendRequest' :: (FromJSON r) => HTTP.Request -> IO (HTTP.Response (Either JSONException r))
sendRequest' = httpJSONEither
