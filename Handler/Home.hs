module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
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
                    wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                    wsAllList <- runDB $ selectList [] [Asc WorkspaceName] 
                    userList <- runDB $ selectList [] [Asc UserUserId] 
                    defaultLayout $ do
                        defaultHeader sessionUserId
                        tabRow "" wsList (unpack role)
                        [whamlet|
                            $if role == "Admin"
                                <div>
                                    <ul ."list">Workspaces
                                        $forall Entity wsKey wsVal <- wsAllList
                                            <li>
                                                <a href="/ws/#{workspaceName wsVal}">#{workspaceName wsVal}
                                <div>
                                    <ul ."list">Users
                                        $forall Entity userKey userVal <- userList
                                            <li>
                                                <a href="/user/#{userUserId userVal}">#{userUserId userVal}
                        |]
                        defaultFooter
