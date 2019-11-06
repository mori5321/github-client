{-# LANGUAGE OverloadedStrings #-}

module GitHub.Env 
    ( getConfig
    , Config(..)
    ) where

import System.Environment (getEnv)
import qualified Data.Text as T

import GitHub.Token (Token)

data Config = MkConfig { token :: Token } deriving Show

getToken :: IO Token
getToken = do
    token <- getEnv "GITHUB_API_TOKEN"
    return $ T.pack token

getConfig :: IO Config
getConfig = MkConfig <$> getToken -- fmap MkConfig getTokenと同義


-- example
-- token <$> getConfig でとれるよ