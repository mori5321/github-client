{-# LANGUAGE OverloadedStrings #-}

module GitHub.Sample where

import           Data.Aeson                     ( Value )
import qualified Data.Yaml                     as Yaml
import qualified Data.ByteString.Char8         as S8

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple            ( setRequestPath
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

import           GitHub.Auth                    ( setRequestAuth
                                                , Auth(..)
                                                )
import           GitHub.Env                     ( getConfig
                                                , Config(..)
                                                )

samp :: IO ()
samp = do
        auth <- OAuth . token <$> getConfig

        let request =
                    setRequestPath (S8.pack $ "/user")
                            $ setRequestMethod "GET"
                            $ setRequestAuth auth -- Authorization
                            $ setRequestHeader "Content-Type"
                                               ["application/json"]
                            $ setRequestHeader "User-Agent" ["morimori"]
                            $ setRequestPort 443
                            $ setRequestSecure True
                            $ setRequestHost "api.github.com"
                            $ defaultRequest

        response' <- httpJSON request
        let body = getResponseBody response' :: Value
        print body


