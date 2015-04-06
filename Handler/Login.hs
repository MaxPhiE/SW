module Handler.Login where

import Import

getLoginR :: Handler Html
getLoginR = do
    (widget, enctype) <- generateFormPost userForm
    defaultLayout $ do
        defaultHeader ""
        [whamlet|
            <table>
                <tbody>
                    <tr>
                        <td>
                            <form method=post action=@{LoginR} enctype=#{enctype}>
                                ^{widget}
                                <button>Submit
        |]
        defaultFooter


postLoginR :: Handler Html
postLoginR = do
    ((result, _), _) <- runFormPost userForm
    case result of
        FormSuccess login -> do
            user <- runDB $ selectFirst [UserUserId ==. userUserId login, UserPassword ==. userPassword login] []
            case user of
                Nothing -> do
                    setMessage "Invalid userid/password."
                    redirect LoginR
                _ -> do
                    setSession "userId" $ userUserId login
                    redirectUltDest HomeR
        _ -> do
            setMessage "Invalid input. (WS-002)"
            redirect LoginR
