{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GitHub.Requests.User.Issues.List
  ( listUserIssues
  , listUserIssuesHttpRequest
  )
where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import Data.Aeson ( Value, FromJSON, ToJSON )
import GHC.Generics
import GitHub.Types.User ( User(..) )
import GitHub.Types.Issue ( Issue(..) )
import GitHub.Query ( toHTTPQueryItem, QueryItem )
import GitHub.Request ( IsRequest(..)
                      , Request(..)
                      , Method(..)
                      , Path
                      , getResponseBody
                      , sendRequest
                      , getResponseStatusCode
                      , mkHttpRequest
                      , withAuth
                      , withBody
                      , withQuery
                      )
import qualified Network.HTTP.Simple as HTTP
import GitHub.Auth ( Auth )

    
mkRequest :: Request
mkRequest = Request { reqPath =  mconcat ["user", "/issues"]
                    , reqMethod = GET
                    }

listUserIssuesHttpRequest :: [QueryItem] -> Auth -> HTTP.Request
listUserIssuesHttpRequest queryItems auth = withAuth auth
                                           . withQuery query
                                           . mkHttpRequest $ req
  where
    req = mkRequest
    query = map toHTTPQueryItem queryItems

listUserIssues :: [QueryItem] -> Auth -> IO [Issue]
listUserIssues queryItems auth =
    getResponseBody <$> sendRequest httpReq
  where
    httpReq = listUserIssuesHttpRequest queryItems auth