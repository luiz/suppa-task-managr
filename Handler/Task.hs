{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Task where

import Import
import Debug.Trace (trace)

getNewTaskR :: Handler Html
getNewTaskR = do
    ((_, formWidget), formEnctype) <- runFormPost taskForm
    defaultLayout $ do
        setTitle "New Task"
        $(widgetFile "newTask")

postNewTaskR :: Handler Html
postNewTaskR = do
    ((result, formWidget), formEnctype) <- runFormPost taskForm
    case result of
        FormSuccess task -> do
            _ <- runDB $ insert task
            redirect $ HomeR
        _ ->
            defaultLayout $ $(widgetFile "newTask")

getTaskR :: TaskId -> Handler Html
getTaskR taskId = do
    task <- runDB $ get404 taskId
    defaultLayout $ do
        setTitle "A Task"
        $(widgetFile "showTask")

deleteTaskR :: TaskId -> Handler Html
deleteTaskR taskId = defaultLayout $ notFound

taskForm :: Form Task
taskForm = renderBootstrap3 $ Task
    <$> areq textField (bs3 "Task description") Nothing
    <*> aopt dayField (bs3 "Due to") Nothing
