{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GitHub.Requests.User.Issues.List
  ( listUserIssues
  , listUserIssuesHttpRequest
  )
where

import GHC.Generics
import qualified Data.ByteString.Char8 as S8
import qualified Network.HTTP.Simple as HTTP

import GitHub.Types.User ( User(..) )
import GitHub.Types.Issue ( Issue(..) )
import GitHub.Query ( toHTTPQueryItem, QueryItem )
import GitHub.Request ( Request(..)
                      , Method(..)
                      , getResponseBody
                      , sendRequest
                      , sendRequest'
                      , getResponseStatusCode
                      , mkHttpRequest
                      , withAuth
                      , withBody
                      , withQuery
                      )
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


listUserIssues :: [QueryItem] -> Auth -> IO (Either Int [Issue])
listUserIssues queryItems auth = do
    res <- getResponseBody <$> sendRequest' httpReq
    case res of
      Right a -> return $ Right a
      Left (HTTP.JSONConversionException a b c) -> return $ Left $ HTTP.getResponseStatusCode b
  where
    httpReq = listUserIssuesHttpRequest queryItems auth