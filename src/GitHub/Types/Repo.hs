{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.Types.Repo
    ( Repo(..)
    )
where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                     as T
import           GHC.Generics
import qualified Data.Text                     as T
import qualified Data.ByteString.Char8         as S8

import           GitHub.Query                   ( IsHTTPQueryItem(..)
                                                , QueryItem
                                                )

data Repo = Repo { id :: Integer
                 } deriving (Show, Generic)
instance FromJSON Repo
instance ToJSON Repo
