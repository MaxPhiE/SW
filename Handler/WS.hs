module Handler.WS where

import Import
import Data.Text (splitOn)
import qualified Prelude as P

getWsR :: Text -> Handler Html
getWsR requestedWsName = do
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
                    requestedWorkspace <- runDB $ getBy $ UniqueName requestedWsName
                    case requestedWorkspace of
                        Nothing         -> do
                            setMessage "The requested workspace doesn't exist."
                            redirect HomeR
                        Just requestedWs -> do
                            wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                            assignment <- runDB $ selectFirst [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey requestedWs] []
                            case assignment of
                                Nothing     -> do
                                    setMessage "You don't have the privilege to see this workspace."
                                    redirect HomeR
                                Just a      -> do
                                    let note = workspaceNote $ entityVal requestedWs
                                    let right = assignmentRight $ entityVal a
                                    defaultLayout $ do
                                        defaultHeader sessionUserId
                                        tabRow (unpack requestedWsName) wsList (unpack role)
                                        workspaceWorkForm (entityVal requestedWs) (unpack right)
                                        defaultFooter
                                        
                                        
postWsR :: Text -> Handler Html
postWsR requestedWorkspaceName = do
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> redirect LoginR
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    setMessage "Unexpected error. (WS-007)"
                    redirect $ WsR requestedWorkspaceName
                Just u  -> do
                    requestedWorkspace <- runDB $ getBy $ UniqueName requestedWorkspaceName
                    case requestedWorkspace of
                        Nothing             -> redirect HomeR
                        Just requestedWs    -> do
                            let status = workspaceStatus $ entityVal requestedWs
                            let note = workspaceNote $ entityVal requestedWs
                            let assignee = workspaceAssignee $ entityVal requestedWs
                            newNote <- fmap getText (runInputPost $ iopt textareaField "noteArea")
                            isGet <- runInputPost $ iopt textField "get"
                            isCheckIn <- runInputPost $ iopt textField "checkIn"
                            isCheckOut <- runInputPost $ iopt textField "checkOut"
                            isDiscardCheckOut <- runInputPost $ iopt textField "discardCheckOut"
                            isDelete <- runInputPost $ iopt textField "delete"
                            wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                            assignment <- runDB $ selectFirst [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey requestedWs] []
                            case assignment of
                                Nothing -> do
                                    setMessage "Unexpected error. (WS-008)"
                                    redirect $ WsR requestedWorkspaceName
                                Just a  -> do
                                    let right = assignmentRight $ entityVal a
                                    case isGet of
                                        Just g -> do
                                            setMessage "Get done"
                                            redirect $ WsR requestedWorkspaceName
                                        _ -> case isCheckIn of
                                            Just i -> do
                                                case unpack status of
                                                    "0"       -> do
                                                        setMessage "Please check out first"
                                                        redirect $ WsR requestedWorkspaceName
                                                    "1"  -> do
                                                        if assignee == sessionUserId
                                                            then do
                                                                liftIO $ writeWSFile (unpack requestedWorkspaceName) newNote (unpack assignee)
                                                                runDB $ updateWhere [WorkspaceName ==. requestedWorkspaceName] [WorkspaceStatus =. pack "0", WorkspaceAssignee =. pack "null", WorkspaceNote =. pack newNote]
                                                                setMessage "Checked in"
                                                                redirect $ WsR requestedWorkspaceName
                                                            else do
                                                                setMessage $ toHtml ("The file is already checked out by " ++ (unpack assignee))
                                                                redirect $ WsR requestedWorkspaceName
                                            _ -> case isCheckOut of
                                                Just o -> do
                                                     case unpack status of
                                                        "0" -> do
                                                            if right == pack "ReadOnly"
                                                                then do
                                                                    setMessage "You do not have the right to check out the note."
                                                                    redirect $ WsR requestedWorkspaceName
                                                                else do
                                                                    runDB $ updateWhere [WorkspaceName ==. requestedWorkspaceName] [WorkspaceStatus =. pack "1", WorkspaceAssignee =. sessionUserId]
                                                                    setMessage "Checked out"
                                                                    redirect $ WsR requestedWorkspaceName   
                                                        _  -> do
                                                            setMessage $ toHtml ("The file is already checked out by " ++ (unpack assignee))
                                                            redirect $ WsR requestedWorkspaceName
                                                _ -> case isDiscardCheckOut of
                                                    Just c -> do
                                                        runDB $ updateWhere [WorkspaceName ==. requestedWorkspaceName] [WorkspaceStatus =. pack "0", WorkspaceAssignee =. "null"]
                                                        setMessage "Check out discarded"
                                                        redirect $ WsR requestedWorkspaceName
                                                    _ -> case isDelete of
                                                        Just d -> do
                                                            runDB $ deleteWhere [AssignmentWorkspaceId ==. entityKey requestedWs]
                                                            runDB $ deleteWhere [WorkspaceName ==. requestedWorkspaceName]
                                                            redirect HomeR
                                                        _ -> do
                                                            setMessage "Unexpected error. (WS-009)"
                                                            redirect $ WsR requestedWorkspaceName


getWsRestR :: Text -> Handler TypedContent
getWsRestR input = do
    let requestedWsName = (splitOn "&" input)P.!!0
    let action          = (splitOn "&" input)P.!!1
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "001" "Not logged in" (Workspace "" "" "" "")
        Just sessionUserId  -> case action of
            "get" -> do
                uid <- runDB $ getBy $ UniqueUserId sessionUserId
                case uid of
                    Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "100" "Unexpected error." (Workspace "" "" "" "")
                    Just u  -> do
                        let role = userRole $ entityVal u
                        requestedWorkspace <- runDB $ getBy $ UniqueName requestedWsName
                        case requestedWorkspace of
                            Nothing         -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "123" (pack ("The requested workspace '" ++ (unpack requestedWsName) ++ "' doesn' exist")) (Workspace "" "" "" "")
                            Just requestedWs -> do
                                wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                                assignment <- runDB $ selectFirst [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey requestedWs] []
                                case assignment of
                                    Nothing     -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "122" "You do not have the privilege to see the note." (Workspace "" "" "" "")
                                    Just a      -> do
                                        selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "000" "Get" (entityVal requestedWs)
            "checkOut" -> do
                uid <- runDB $ getBy $ UniqueUserId sessionUserId
                case uid of
                    Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "100" "Unexpected error." (Workspace "" "" "" "")
                    Just u  -> do
                        requestedWorkspace <- runDB $ getBy $ UniqueName requestedWsName
                        case requestedWorkspace of
                            Nothing             -> redirect HomeR
                            Just requestedWs    -> do
                                let note = workspaceNote $ entityVal requestedWs
                                let status = workspaceStatus $ entityVal requestedWs
                                let assignee = workspaceAssignee $ entityVal requestedWs
                                wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                                assignment <- runDB $ selectFirst [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey requestedWs] []
                                case assignment of
                                    Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "122" "You do not have the privilege to see the note." (Workspace "" "" "" "")
                                    Just a  -> do
                                        let right = assignmentRight $ entityVal a
                                        case unpack status of
                                            "0" -> do
                                                if right == pack "ReadOnly"
                                                    then selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "121" "You do not have the privilege to check out the note." (entityVal requestedWs)
                                                    else do
                                                        runDB $ updateWhere [WorkspaceName ==. requestedWsName] [WorkspaceStatus =. pack "1", WorkspaceAssignee =. sessionUserId]
                                                        selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "000" "Checked out" (entityVal requestedWs)
                                            _  -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "120" (pack ("The file is already checked out by " ++ (unpack assignee))) (entityVal requestedWs)
            _ -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "200" (pack ("Action '" ++ (unpack action) ++ "' not supported")) (Workspace "" "" "" "")
                                        
                                        

postWsRestR :: Text -> Handler TypedContent
postWsRestR input = do
    let requestedWsName = (splitOn "&" input)P.!!0
    let action          = (splitOn "&" input)P.!!1
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "001" "Not logged in" (Workspace "" "" "" "")
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "100" "Unexpected error." (Workspace "" "" "" "")
                Just u  -> do
                    requestedWorkspace <- runDB $ getBy $ UniqueName requestedWsName
                    case requestedWorkspace of
                        Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "123" (pack ("The requested workspace '" ++ (unpack requestedWsName) ++ "' doesn' exist")) (Workspace "" "" "" "")
                        Just requestedWs    -> do
                            let status = workspaceStatus $ entityVal requestedWs
                            let note = workspaceNote $ entityVal requestedWs
                            let assignee = workspaceAssignee $ entityVal requestedWs
                            let newNote = (splitOn "&" input)P.!!2
                            wsList <- fmap (map fst) (queryWorkspaceAssignmentByUser u)
                            assignment <- runDB $ selectFirst [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey requestedWs] []
                            case assignment of
                                Nothing -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "122" "You do not have the privilege to see the note." (Workspace "" "" "" "")
                                Just a  -> do
                                    let right = assignmentRight $ entityVal a
                                    do
                                        case unpack status of
                                            "0"       -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "124" "Please check out first" (entityVal requestedWs)
                                            "1"  -> do
                                                if assignee == sessionUserId
                                                    then do
                                                        case action of
                                                            "checkIn" -> do
                                                                liftIO $ writeWSFile (unpack requestedWsName) (unpack newNote) (unpack assignee)
                                                                runDB $ updateWhere [WorkspaceName ==. requestedWsName] [WorkspaceStatus =. pack "0", WorkspaceAssignee =. pack "null", WorkspaceNote =. newNote]
                                                                selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "000" "Checked in" (entityVal requestedWs)
                                                            "save" -> do
                                                                liftIO $ writeWSFile (unpack requestedWsName) (unpack newNote) (unpack assignee)
                                                                runDB $ updateWhere [WorkspaceName ==. requestedWsName] [WorkspaceNote =. newNote]
                                                                selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "000" "Checked in & Checked out" (entityVal requestedWs)
                                                            _ -> selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "200" (pack ("Action '" ++ (unpack action) ++ "' not supported")) (Workspace "" "" "" "")
                                                    else selectRep $ provideRep $ returnJson $ toJSON $ MyResponse "000" (pack ("The file is already checked out by " ++ (unpack assignee))) (entityVal requestedWs)


