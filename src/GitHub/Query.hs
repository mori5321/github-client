{-# LANGUAGE ExistentialQuantification #-}

module GitHub.Query
        ( IsHTTPQueryItem(..)
        , QueryItem(..)
        )
where

import qualified Network.HTTP.Simple           as HTTP

class IsHTTPQueryItem a where
    toHTTPQueryItem :: a -> HTTP.QueryItem


-- QueryItem is a HeteroList which is made by GATDs
-- Usage
-- 
-- auth <- authorize
-- query = [QueryItem Created, Query Open]
-- listUserIssues query auth
data QueryItem = forall a . IsHTTPQueryItem a => QueryItem a
instance IsHTTPQueryItem QueryItem where
        toHTTPQueryItem (QueryItem qi) = toHTTPQueryItem qi
