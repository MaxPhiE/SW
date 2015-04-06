module Handler.MyUser where

import Import

getMyUserR :: Handler Html
getMyUserR = do
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
                    let role = userRole $ entityVal u
                    wsAssList <- queryWorkspaceAssignmentByUser u
                    defaultLayout $ do
                        defaultHeader sessionUserId
                        tabRow "" (map fst wsAssList) (unpack role)
                        [whamlet| <div ."subTitleClass" id="userHeader">User: #{unpack $ userUserId $ entityVal u} |]
                        myUserForm (unpack $ userUserId $ entityVal u) (unpack $ userRole $ entityVal u)
                        [whamlet|
                            <div ."subTitleClass" id="assignmentHeader">Assignments
                            <table id="assignmentTable">
                                <tbody>
                                    $forall (Entity _ wsVal, Entity _ assVal) <- wsAssList
                                        <tr>
                                            <td>#{workspaceName wsVal}
                                            <td>#{assignmentRight assVal}
                        |]
                        [whamlet|
                            <div ."subTitleClass" id="workspaceHeader">Workspace Order
                            <table id="workspaceOrderTable">
                                <tbody>
                                    <tr>
                                        <td>
                                            <select id="workspaceOrder" multiple="yes" size="5">
                                                $forall (Entity _ wsVal, Entity _ _) <- wsAssList
                                                    <option value="#{workspaceName wsVal}">#{workspaceName wsVal}
                                        <td>
                                            <input type="button" ."btnShort" id="moveUp" name="moveUp" value="Up" onclick="moveUp();">
                                            <input type="button" ."btnShort" id="moveDown" name="moveDown" value="Down" onclick="moveDown();">
                                            <input type="button" ."btnShort" id="saveWSOrder" name="save" value="Save" onclick="saveWSOrder();">
                        |]
                        defaultFooter


postMyUserR :: Handler Html
postMyUserR = do
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-001)"
                    redirect LoginR
                Just u  -> do
                    password <- runInputPost $ ireq textField "newPassword"
                    passwordR <- runInputPost $ ireq textField "newPasswordR"
                    if password /= passwordR
                        then do
                            setMessage "New password and repeated new password are not equal."
                            redirect MyUserR
                        else do    
                            runDB $ updateWhere [UserUserId ==. (userUserId $ entityVal u)] [UserPassword =. password]
                            setMessage "Password updated"
                            redirect MyUserR
