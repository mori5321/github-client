{-# LANGUAGE DuplicateRecordFields #-}

module GitHub.Requests.User
    ( listUserIssues
    , getUser
    , listUsers
    )
where

import GitHub.Requests.User.Get (getUser)
import GitHub.Requests.User.List (listUsers)
import GitHub.Requests.Issues.List (listUserIssues)