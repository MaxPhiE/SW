module Handler.NewUser where

import Import

getNewUserR :: Handler Html
getNewUserR = do
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
                            setMessage "You are not allowed to create new user."
                            redirect HomeR
                        else do
                            wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                            defaultLayout $ do
                                defaultHeader sessionUserId
                                tabRow "" wsList role
                                userNewForm
                                defaultFooter


postNewUserR :: Handler Html
postNewUserR = do
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> redirect LoginR
        Just sessionUserId  -> do
            userId <- runInputPost $ ireq textField "userId"
            role <- runInputPost $ ireq textField "role"
            isSave <- runInputPost $ iopt textField "save"
            isSaveAndNew <- runInputPost $ iopt textField "saveAndNew"
            if isJust2 isSave isSaveAndNew
                then do
                    newUser <- runDB $ insertUnique $ User userId userId role
                    case newUser of
                        Nothing     -> do
                            setMessage "UserId already in use."
                            redirect NewUserR
                        Just newU   -> do
                            setMessage "User created"
                            if isSave /= Nothing
                                then redirect $ UserR userId
                                else redirect NewUserR
                else do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
