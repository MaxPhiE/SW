module Handler.WSOrder where

import Import
import Data.Text (splitOn)

postWsOrderR :: Text -> Handler RepPlain
postWsOrderR input = do
    let inputA = (splitOn "&" input)
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> return $ RepPlain "Not logged in. (WS-000)"
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    return $ RepPlain "Unexpected error. (WS-001)"
                Just u  -> do
                    updateWsOrder inputA u


updateWsOrder :: [Text] -> (Entity User) -> Handler RepPlain
updateWsOrder (wsName:orderWs:inputA) u = do
    workspace <- runDB $ getBy $ UniqueName wsName
    case workspace of
        Nothing -> return $ RepPlain "Unexpected error. (WS-009)"
        Just ws -> do
            runDB $ updateWhere [AssignmentUserId ==. entityKey u, AssignmentWorkspaceId ==. entityKey ws] [AssignmentWsOrder =. orderWs]
            updateWsOrder inputA u
updateWsOrder _ _ = return $ RepPlain "OK"
