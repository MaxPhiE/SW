{-# LANGUAGE OverloadedStrings          #-}

module Handler.AssignUser where

import Import
import Data.Text                (splitOn)
import qualified Prelude        as P

postAssignUserR :: Text -> Text -> Handler RepPlain
postAssignUserR requestedUserId input = do
    let workspace   = P.head (splitOn "&" input)
    let right       = P.last (splitOn "&" input)
    sessionUserIdM <- lookupSession "userId"
    case sessionUserIdM of
        Nothing             -> return $ RepPlain "Not logged in. (WS-000)"
        Just sessionUserId  -> do
            uid <- runDB $ getBy $ UniqueUserId sessionUserId
            case uid of
                Nothing -> do
                    return $ RepPlain "Unexpected error. (WS-001)"
                Just u  -> do
                    requestedUser <- runDB $ getBy $ UniqueUserId requestedUserId
                    case requestedUser of
                        Nothing         -> do
                            return $ RepPlain "Unexpected error. (WS-004)"
                        Just requestedU -> do
                            wsM <- runDB $ getBy $ UniqueName workspace
                            case wsM of
                                Nothing -> do
                                    return $ RepPlain "Workspace doesn't exist."
                                Just ws -> do                                    
                                    if (unpack right) == "Nothing"
                                        then do
                                            deleteAss <- runDB $ selectFirst [AssignmentUserId ==. entityKey requestedU, AssignmentWorkspaceId ==. entityKey ws] []
                                            case deleteAss of
                                                Nothing         -> do
                                                    return $ RepPlain "Unexpected error. (WS-005)"
                                                Just deleteA    -> do
                                                    runDB $ delete (entityKey deleteA)
                                                    return $ RepPlain "OK"
                                        else do
                                            newAss <- runDB $ insertUnique $ Assignment (entityKey requestedU) (entityKey ws) right "0"
                                            case newAss of
                                                Just newA   -> return $ RepPlain "OK"
                                                Nothing     -> do
                                                    runDB $ updateWhere [AssignmentWorkspaceId ==. entityKey ws] [AssignmentRight =. right]
                                                    return $ RepPlain "OK"
