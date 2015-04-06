module Handler.DeleteUser where

import Import

getDeleteUserR :: Text -> Handler Html
getDeleteUserR requestedUserId = do
    clearUltDest
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> do
            setUltDestCurrent
            redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    let role = unpack $ userRole $ entityVal u
                    if role /= "Admin"
                        then do
                            setMessage "You don't have the privilege to delete this user profile."
                            redirect HomeR
                        else do
                            requestedUser <- runDB $ getBy $ UniqueUserId requestedUserId
                            case requestedUser of
                                Nothing         -> do
                                    setMessage "Unexpected error. (WS-006)"
                                    redirect $ UserR requestedUserId
                                Just requestedU -> do
                                    runDB $ deleteWhere [AssignmentUserId ==. entityKey requestedU]
                                    runDB $ delete $ entityKey requestedU
                                    redirect HomeR
