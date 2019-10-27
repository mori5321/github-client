{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module GitHub.Error
    ( parseError
    , Error(..)
    , Body(..)
    )
where

import GHC.Generics
import Network.HTTP.Simple ( JSONException (..), getResponseBody, getResponseStatus)
import qualified Network.HTTP.Types.Status as Status
import qualified Network.HTTP.Simple as HTTP
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as S8
import Data.Aeson (fromJSON, ToJSON, FromJSON, Result(Success))
import qualified Data.Map as M
import Control.Applicative

data Error = Error { statusCode :: Int
                   , statusMessage :: T.Text
                   , responseBody :: Body
                   } deriving (Show, Generic, FromJSON, ToJSON)

data Body = ErrorBody { message :: T.Text
                      } deriving (Show, Generic, FromJSON, ToJSON)
                            

parseError :: HTTP.JSONException -> Error
parseError (JSONConversionException _ res _) =
    Error { statusCode = statusCode
          , statusMessage = statusMessage
          , responseBody = body }
  where
    status = getResponseStatus res
    statusCode = Status.statusCode status
    statusMessage = T.pack . S8.unpack $ Status.statusMessage status
    Success body = fromJSON $ getResponseBody res
    
-- where
-- -- statusCode = getResponseStatusCode :: int
-- error = fromJSON $ getResponseBody res

    