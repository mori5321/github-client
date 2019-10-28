{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GitHub.Requests.Issues.List
  ( listIssues
  , listIssuesHttpRequest
  , listUserIssues
  , listUserIssuesHttpRequest
  , listOrganizationIssues
  , listOrganizationIssuesHttpRequest
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
import GitHub.Error (Error, parseBodyEither, getResponseBodyEither)

listIssuesHttpRequest :: [QueryItem] -> Auth -> HTTP.Request
listIssuesHttpRequest queryItems auth = withAuth auth
                                        . withQuery query
                                        . mkHttpRequest $ req
  where
    req = Request { reqPath = mconcat ["/issues"]
                  , reqMethod = GET }
    query = map toHTTPQueryItem queryItems

                             
listIssues :: [QueryItem] -> Auth -> IO (Either Error [Issue])
listIssues queryItems auth =
    getResponseBodyEither <$> sendRequest' httpReq
   where
    httpReq = listIssuesHttpRequest queryItems auth



listUserIssuesHttpRequest :: [QueryItem] -> Auth -> HTTP.Request
listUserIssuesHttpRequest queryItems auth = withAuth auth
                                           . withQuery query
                                           . mkHttpRequest $ req
  where
    req = Request { reqPath =  mconcat ["user", "/issues"]
                  , reqMethod = GET }
    query = map toHTTPQueryItem queryItems

-- # Usage
-- Right res <- listUserIssues [QueryItem Created] auth
-- mapM_ Data.Text.IO.putStrLn $ map GitHub.Types.Issue.title res
listUserIssues :: [QueryItem] -> Auth -> IO (Either Error [Issue])
listUserIssues queryItems auth =
    getResponseBodyEither <$> sendRequest' httpReq
  where
    httpReq = listUserIssuesHttpRequest queryItems auth



type OrganizationName = S8.ByteString

listOrganizationIssuesHttpRequest :: OrganizationName ->
                                     [QueryItem] ->
                                     Auth ->
                                     HTTP.Request
listOrganizationIssuesHttpRequest name queryItems auth =
    withAuth auth . withQuery query . mkHttpRequest $ req
  where
    req = Request { reqPath = S8.intercalate "/" ["orgs", name, "issues"]
                  , reqMethod = GET }
    query = map toHTTPQueryItem queryItems


listOrganizationIssues :: OrganizationName ->
                          [QueryItem] ->
                          Auth ->
                          IO (Either Error [Issue])
listOrganizationIssues name queryItems auth =
    getResponseBodyEither <$> sendRequest' httpReq
  where
    httpReq = listUserIssuesHttpRequest queryItems auth
