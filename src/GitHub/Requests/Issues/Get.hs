{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module GitHub.Requests.Issues.Get
  ( getIssue
  , getIssueHttpRequest
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

type OwnerName = S8.ByteString
type RepoName = S8.ByteString
type IssueNumber = Int

getIssueHttpRequest :: OwnerName ->
                       RepoName ->
                       IssueNumber ->
                       Auth ->
                       HTTP.Request
getIssueHttpRequest ownerName repoName issueNumber auth =
    withAuth auth . mkHttpRequest $ req
  where
    req = Request { reqPath = S8.intercalate "/" ["repos", ownerName, repoName, "issues", S8.pack . show $ issueNumber]
                  , reqMethod = GET
                  }

getIssue :: OwnerName ->
            RepoName ->
            IssueNumber ->
            Auth ->
            IO (Either Error Issue)
getIssue ownerName repoName issueNumber auth =
    getResponseBodyEither <$> sendRequest' httpReq
  where
    httpReq = getIssueHttpRequest ownerName repoName issueNumber auth