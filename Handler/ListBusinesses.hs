{-# LANGUAGE OverloadedStrings #-}
module Handler.ListBusinesses where

import Import hiding (pack)
import Text.Read (readEither)
import Data.Text (pack)

data LinkUrls = LinkUrls
  { self  :: Text
  , first :: Text
  , prev  :: Text
  , next  :: Text
  , last  :: Text
  } deriving Show

type Links = [(Int,LinkUrls)]

getListBusinessesR :: Handler (Value)
getListBusinessesR =  do
  (App app_settings _ _ _) <- getYesod
  page_number' <- lookupGetParams "page[number]"
  page_size'   <- lookupGetParams "page[size]"
  host             <- lookupHeader "Host" 
  liftIO $ putStrLn (" page number is " ++ (pack $ show page_number'))
  liftIO $ putStrLn (" page size is " ++ (pack $ show page_size'))
  liftIO $ putStrLn (" host is " ++ (pack $ show (host))) 
  -- ^^ Just "localhost:3000"

  pages <- getPage (page_number page_number') (page_size page_size')
  meta  <- getMeta (page_number page_number') (page_size page_size') host 
  return $ object [("businesses" ,toJSON pages)]
  where
    page_number pn = case pn of
      []      -> error ("JSON error to be implemented")
      (pn':_) -> fromText pn'
    page_size ps   = case ps of
      []      -> error ("JSON error to be implemented")
      (ps':_) -> fromText ps'
      
getPage :: Int -> Int -> Handler [Entity Businesses]
getPage page_number page_size = do
  page <- 
    runDB $ do selectList [BusinessesId' >=. first_key, BusinessesId' <=. last_key] [LimitTo page_size]
  return page
  where
    first_key = ((page_number - 1) * page_size) 
    last_key  = (first_key + (page_size - 1))
    toBusinesses (Entity _ business) = business

getMeta :: Int -> Int -> Maybe ByteString -> Handler () -- [Links]
getMeta page_number page_size = do
  meta <-
    runDB $ do selectList [PaginationId >=. first_key, PaginationId <=. last_key] [LimitTo page_size]
  return ()
  where
    first_key = ((page_number - 1) * page_size)  
    last_key  = (first_key + (page_size - 1))
     

-- goes in own module
fromText :: Text -> Int
fromText str_int = toInteger :: Int
  where
    toInteger =
      case (readEither (unpack str_int) :: Either String Int) of
        (Left err_msg) -> error ("Hard Fail key value : " ++ " " ++ (unpack str_int) ++ err_msg)
        (Right int)    -> int


