module Handler.Logout where

import Import

getLogoutR :: Handler Html
getLogoutR = do
    clearUltDest
    setMessage "Logged out"
    deleteSession "userId"
    redirect LoginR
