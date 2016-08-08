{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Handler.ListBusinesses where

import Import hiding (fromList,pack)
import Text.Read (readEither)
import Data.Either.Utils (maybeToEither)
import Data.Maybe (fromJust) -- alert alert FIXME
import Data.HashMap.Strict hiding (map)
import Data.Text (pack)
import Data.Aeson 
-- import Data.Text.Encoding
import Database.Persist.Sql (toSqlKey,fromSqlKey)

data LinkUrls = LinkUrls
  { self  :: Text
  , first :: Text
  , prev  :: Text
  , next  :: Text
  , last  :: Text
  } deriving (Generic,Show)

type LinkMap = [(LinkId, LinkUrls)]
type PageNumber = Int
type PageSize   = Int
type PageNumber64 = Int64
type PageSize64   = Int64
type KeyVal       = Int64
type Host       = Text
newtype LinkId = LinkId {linkID :: Int64} deriving (Eq,Ord,Generic,Hashable)
instance ToJSON LinkId
instance ToJSON LinkUrls


getListBusinessesR :: Handler (Value)
getListBusinessesR =  do
  page_number' <- lookupGetParams "page[number]"
  page_size'   <- lookupGetParams "page[size]"
  host'        <- lookupHeader "Host" -- FIXME
  let host = decodeUtf8 (fromMaybe host_err host') 
  -- ^^ Just "localhost:3000"

  pages <- getPage (page_number page_number') (page_size page_size')
  meta  <- getMeta (page_number page_number') (page_size page_size') host 
  return $ object [("businesses" ,toJSON pages),("meta", toJSON meta)]
  where
    host_err = "Host Not Found In Header"
    page_number pn = case pn of
      []      -> error ("JSON error to be implemented")
      (pn':_) -> fromText pn'
    page_size ps   = case ps of
      []      -> error ("JSON error to be implemented")
      (ps':_) -> fromText ps'
      
getPage :: PageNumber -> PageSize -> Handler [Businesses]
getPage page_number page_size = do
  pages <- runDB $ do select_range
  return (map entityVal pages)
  where
    select_range = 
     selectList 
       [BusinessesId' >=. lb_key, BusinessesId' <=. ub_key] [LimitTo page_size]
    lb_key = ((page_number - 1) * page_size)
    ub_key  = (lb_key + (page_size - 1)) 

getMeta :: PageNumber -> PageSize -> Host -> Handler LinkMap
getMeta page_number' page_size' host = do
  meta@(fp:_) <- runDB $ do select_range
  return $ 
    map (toLinks lb_key' ub_key' (finalKey fp) page_number page_size host) $
    map entityVal meta
  where
    lb_key' = ((page_number - 1) * page_size)
    lb_key  = toSqlKey lb_key' :: Key Pagination
    ub_key' = (lb_key' + (page_size - 1))
    ub_key  = toSqlKey ub_key' :: Key Pagination
    page_number = fromIntegral page_number' :: Int64
    page_size   = fromIntegral page_size'   :: Int64
    select_range = 
      selectList 
        [PaginationId >=. lb_key, PaginationId <=. ub_key] [LimitTo page_size']

toLinks :: KeyVal           ->
           KeyVal           ->
           KeyVal           ->
           PageNumber64     ->
           PageSize64       ->
           Host             ->
           Pagination       ->
           (LinkId,LinkUrls)
toLinks lb_key ub_key final_key page_number' page_size' host (Pagination self first prev next last) = 
  (LinkId sid,(LinkUrls self' first' prev' next' last'))
  where
    sid = fromSqlKey self
    self'  = link page_number' 
    first' = link 1 
    prev'  = 
      let pn = if ((fromSqlKey (fromJust prev)) < lb_key) 
               then (page_number' - 1) 
               else page_number'
      in link pn 
    next'  = 
      let pn = if ((fromSqlKey (fromJust next)) > ub_key)
               then (page_number' + 1)
               else page_number'
      in link pn
    last' =
      let pn = final_key `div` page_size' + (final_key `mod` page_size')
      in link pn
    page_number   = fromIntegral page_number' :: Int64
    link pn = 
      "http://"                   ++ 
      host                        ++
      "/businesses?page[number]=" ++ 
      (pack $ show pn)            ++
      "&page[size]="              ++
      (pack $ show page_size')
-- goes in own module
finalKey :: Entity Pagination -> Int64
finalKey fp = fromSqlKey (fromJust $ paginationLast (entityVal fp))

fromText :: Text -> Int
fromText str_int = toInteger :: Int
  where
    toInteger =
      case (readEither (unpack str_int) :: Either String Int) of
        (Left err_msg) -> error ("Hard Fail key value : " ++ " " ++ (unpack str_int) ++ err_msg)
        (Right int)    -> int


