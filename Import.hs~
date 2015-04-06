module Import
    ( module Import
    ) where

import Foundation                as Import
import Import.NoFoundation       as Import
import Database.Persist.Sqlite   as Import
import Data.Text                 (length)


{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - help functions ------------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}

queryWorkspaceAssignmentByUserStmt :: Text
queryWorkspaceAssignmentByUserStmt = "SELECT ??, ?? FROM Workspace, Assignment ON Workspace.id = Assignment.workspace_id WHERE Assignment.user_id = ? ORDER BY Assignment.ws_order"

queryWorkspaceAssignmentByUserOuterStmt :: Text
queryWorkspaceAssignmentByUserOuterStmt = "SELECT ??, a.right FROM Workspace LEFT OUTER JOIN (SELECT Assignment.workspace_id, Assignment.right FROM Assignment WHERE Assignment.user_id = ?) a ON Workspace.id = a.workspace_id"


queryWorkspaceAssignmentByUser :: Entity User -> Handler [(Entity Workspace, Entity Assignment)]
queryWorkspaceAssignmentByUser user = runDB $ rawSql queryWorkspaceAssignmentByUserStmt [toPersistValue $ entityKey user]


queryWorkspaceAssignmentByUserOuter :: Entity User -> Handler [(Entity Workspace, Maybe (Single Text))]
queryWorkspaceAssignmentByUserOuter user = runDB $ rawSql queryWorkspaceAssignmentByUserOuterStmt [toPersistValue $ entityKey user]


writeWSFile :: String -> String -> String -> IO ()
writeWSFile _ _ _ = return ()
{-
writeWSFile name note userid = do 
    time <- getClockTime
    appendFile ("log/" ++ name ++ ".log") ("*************** " ++ (show time) ++ " ***** " ++ userid ++ " ***************\n\n" ++ note ++ "\n\n**********************************************************************\n")
    writeFile ("log/" ++ name ++ ".txt") note
-}

getText :: Maybe Textarea -> String
getText (Just text) = unpack $ unTextarea text
getText Nothing     = ""


isJust2 :: Maybe a -> Maybe b -> Bool
isJust2 Nothing Nothing = False
isJust2 _       _       = True



{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - constant variables --------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}



openConnectionCount :: Int
openConnectionCount = 20

title :: Html
title = "Shared Workspace"



{-----------------------------------------------------------------------------
 -----------------------------------------------------------------------------
 - html widgets --------------------------------------------------------------
 -----------------------------------------------------------------------------
 -----------------------------------------------------------------------------}


defaultHeader :: Text -> Widget
defaultHeader sessionUserId = do
    setTitle title
    addScript $ StaticR js_jquery_2_1_3_min_js
    addStylesheet $ StaticR css_ws_css
    [whamlet|<link rel="icon" href=@{StaticR img_favicon_ico} /> |]
    toWidgetHead [hamlet| 
        $doctype 5
        <meta http-equiv="content-type" content="text/html; charset=UTF-8"> |]
    [whamlet|<script> var sessionUserId = "#{sessionUserId}"; |]
    [whamlet| <div ."titleClass" id="title">#{title} |]


tabRow :: String -> [Entity Workspace] -> String -> Widget
tabRow currentWs wsList role = $(widgetFile "tabRow")


defaultFooter :: Widget
defaultFooter = do
    mmsg <- getMessage
    case mmsg of
        Nothing     -> [whamlet| <div ."footerClass" id="footer">&nbsp; |]
        Just msg    -> [whamlet| <div ."footerClass" id="footer">#{msg} |]


userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs $ (\userId pw -> User userId pw "")
    <$> areq textField "UserId" Nothing
    <*> areq passwordField "Password" Nothing


userNewForm :: Widget
userNewForm = $(widgetFile "userNewForm")


userEditForm :: String -> String -> Widget
userEditForm userId role = $(widgetFile "userEditForm")


myUserForm :: String -> String -> Widget
myUserForm userId role = $(widgetFile "myUserForm")


workspaceForm :: Html -> MForm Handler (FormResult Workspace, Widget)
workspaceForm = renderDivs $ (\name -> Workspace name "" "0" "")
    <$> areq shortTextField "Name" Nothing
    where
        shortTextField = check validateLength textField
        validateLength t
            | Data.Text.length t > 20 = Left ("The maximum length for a Workspace Name is 20 characters." :: Text)
            | otherwise     = Right t


workspaceWorkForm :: Workspace -> String -> Widget
workspaceWorkForm (Workspace name note status assignee) right = $(widgetFile "workspace")


