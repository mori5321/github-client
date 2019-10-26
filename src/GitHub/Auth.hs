{-# LANGUAGE OverloadedStrings #-}


module GitHub.Auth
    ( setRequestAuth
    , authorize
    , Auth(..)
    ) where

import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Simple (setRequestHeader, Request)
import GitHub.Env (getConfig, Config(..))
import GitHub.Token (Token)

data Auth = NoAuth | OAuth Token deriving (Show, Eq)

setRequestAuth :: Auth -> Request -> Request
setRequestAuth (OAuth token) = setAuthHeader $ "token " <> token

setAuthHeader :: Token -> Request -> Request
setAuthHeader token = setRequestHeader "Authorization" [token]

authorize :: IO Auth
authorize = OAuth . token <$> getConfig