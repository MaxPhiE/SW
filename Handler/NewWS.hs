module Handler.NewWS where

import Import
import qualified Prelude        as P

getNewWsR :: Handler Html
getNewWsR = do
    clearUltDest
    (widget, enctype) <- generateFormPost workspaceForm
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
                    defaultLayout $ do
                        defaultHeader sessionUserId
                        tabRow "" wsList (unpack role)
                        [whamlet|
                            <table>
                                <tbody>
                                    <tr>
                                        <td>
                                            <form method=post action=@{NewWsR} enctype=#{enctype}>
                                                ^{widget}
                                                <button>Submit
                        |]
                        $(widgetFile "newWs")
                        defaultFooter


postNewWsR :: Handler Html
postNewWsR = do
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
                    ((result, _), _) <- runFormPost workspaceForm
                    case result of
                        FormSuccess ws -> do
                            wsId <- runDB $ insertUnique ws
                            case wsId of
                                Nothing -> do
                                    setMessage "This name is already in use."
                                    redirect NewWsR
                                Just workspace -> do
                                    _ <- runDB $ insert $ Assignment (entityKey u) workspace "Admin" "0"
                                    redirect $ WsR (workspaceName ws)
                        FormFailure msg -> defaultLayout $ do
                            setMessage $ toHtml $ P.head msg
                            redirect NewWsR
                        _ -> defaultLayout $ do
                            setMessage "Invalid input. (WS-002)"
                            redirect NewWsR

