module Handler.User where

import Import

getUserR :: Text -> Handler Html
getUserR requestedUserId = do
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
                            setMessage "You don't have the privilege to see this user profile."
                            redirect HomeR
                        else do
                            requestedUser <- runDB $ getBy $ UniqueUserId requestedUserId
                            case requestedUser of
                                Nothing         -> do
                                    setMessage "The requested user doesn't exist."
                                    redirect HomeR
                                Just requestedU -> do
                                    wsAssList <- queryWorkspaceAssignmentByUserOuter requestedU
                                    wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                                    defaultLayout $ do
                                        defaultHeader sessionUserId
                                        tabRow "" wsList role
                                        [whamlet| <div ."subTitleClass" id="userHeader">User: #{unpack requestedUserId} |]
                                        userEditForm (unpack requestedUserId) (unpack $ userRole $ entityVal requestedU)
                                        [whamlet| <div ."subTitleClass" id="assignmentHeader">Assignments |]
                                        [whamlet|
                                            <table id="assignmentTable">
                                                <tbody>
                                                    $forall (Entity _ wsVal, right) <- wsAssList
                                                        <tr>
                                                            <td>#{workspaceName wsVal}
                                                            <td>
                                                                <select id="right" name="right">
                                                                    <option name="Nothing" value="Nothing">
                                                                    $if right == Just (Single (pack "ReadOnly"))
                                                                        <option name="ReadOnly" value="ReadOnly" selected="selected">ReadOnly
                                                                    $else
                                                                        <option name="ReadOnly" value="ReadOnly">ReadOnly
                                                                    $if right == Just (Single (pack "Edit"))
                                                                        <option name="Edit" value="Edit" selected="selected">Edit
                                                                    $else
                                                                        <option name="Edit" value="Edit">Edit
                                                                    $if right == Just (Single (pack "Admin"))
                                                                        <option name="Admin" value="Admin" selected="selected">Admin
                                                                    $else
                                                                        <option name="Admin" value="Admin">Admin
                                        |]
                                        defaultFooter


postUserR :: Text -> Handler Html
postUserR requestedUserId = do
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
                Just _  -> do
                    requestedUser <- runDB $ getBy $ UniqueUserId requestedUserId
                    case requestedUser of
                        Nothing         -> do
                            setMessage "Unexpected error. (WS-004)"
                            redirect $ UserR requestedUserId
                        Just _ -> do
                            role <- runInputPost $ ireq textField "role"
                            runDB $ updateWhere [UserUserId ==. requestedUserId] [UserRole =. role]
                            redirect $ UserR requestedUserId
