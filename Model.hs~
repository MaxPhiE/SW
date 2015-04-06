{-# LANGUAGE DeriveGeneric #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


data MyResponse = MyResponse
    { myStatusCode    :: Text
    , statusMsg     :: Text
    , workspace     :: Workspace
    } deriving (Generic)
    
instance ToJSON MyResponse

instance ToJSON Workspace where
    toJSON (Workspace name note status assignee) = object 
        [ "name"    .= name
        , "note"    .= note
        , "status"  .= status
        , "assignee".= assignee
        ]
