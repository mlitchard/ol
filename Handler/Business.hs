module Handler.Business where

import Import

getRetrieveBusinessR :: BusinessesId -> Handler (Value)
getRetrieveBusinessR bid =
  returnJson <=< 
  runDB $ do getBy404 (UniqueBusinessesId bid) 
