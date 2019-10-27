{-# LANGUAGE DuplicateRecordFields #-}

module GitHub.Requests.User
    ( listUserIssues
    , fetchUser
    , listUsers
    )
where

import GitHub.Requests.User.Fetch (fetchUser)
import GitHub.Requests.User.List (listUsers)
import GitHub.Requests.User.Issues.List (listUserIssues)