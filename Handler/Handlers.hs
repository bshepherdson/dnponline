{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Handlers where

import DnP

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- DnP.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        setTitle "dnponline homepage"
        addWidget $(widgetFile "homepage")





