{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module GitHub.Types.Issue
    ( Issue(..)
    , Filter(..)
    , State(..)
    , Label(..)
    , Sort(..)
    , Direction(..)
    , Since
    , HeteroQueryItem(..)
    )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics
import qualified Data.ByteString.Char8 as S8
import Data.List
import Data.Time
import Data.Time.ISO8601

import GitHub.Request (  IsQueryItem(..) )


data Issue = Issue { id :: Integer
                   } deriving (Show, Generic, FromJSON, ToJSON)

data Filter = Assigned | Created | Mentioned | Subscribed | All
instance IsQueryItem Filter where
    toQueryItem Assigned   = ("filter", Just "assigned")
    toQueryItem Created    = ("filter", Just "created")
    toQueryItem Mentioned  = ("filter", Just "mentioned")
    toQueryItem Subscribed = ("filter", Just "subscribed")
    toQueryItem All        = ("filter", Just "all")

data State = Open | Closed | AllState
instance IsQueryItem State where
    toQueryItem Open     = ("state", Just "open")
    toQueryItem Closed   = ("state", Just "closed")
    toQueryItem AllState = ("state", Just "all")

newtype Label = Label S8.ByteString
type Labels = [Label]

unwrapLabel :: Label -> S8.ByteString
unwrapLabel (Label bs) = bs

instance IsQueryItem Labels where
    toQueryItem labels = ("labels", Just $ S8.intercalate "," $ map unwrapLabel labels)

data Sort = SortByCreated | SortByUpdated | SortByComments
instance IsQueryItem Sort where
    toQueryItem SortByCreated  = ("sort", Just "created")
    toQueryItem SortByUpdated  = ("sort", Just "updated")
    toQueryItem SortByComments = ("sort", Just "comments")

data Direction = Desc | Asc
instance IsQueryItem Direction where
    toQueryItem Desc = ("direction", Just "desc")
    toQueryItem Asc = ("direction", Just "asc")

type Since = UTCTime
instance IsQueryItem Since where
    toQueryItem since = ("since", Just $ S8.pack $ formatISO8601 since)

data HeteroQueryItem = forall a . IsQueryItem a => HeteroQueryItem a
instance IsQueryItem HeteroQueryItem where
    toQueryItem (HeteroQueryItem qi) = toQueryItem qi