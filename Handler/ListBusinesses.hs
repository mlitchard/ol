{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
{- module: Handler.ListBusinesses
   handles multiple business listings
-}
module Handler.ListBusinesses where

import Safe
import Text.Read (readEither)
import Data.Text (pack)
import Database.Persist.Sql (toSqlKey,fromSqlKey)

import Import hiding (fromList,pack,toInteger)
import BusinessTypes

-- | getListBusinessesR
-- gets initial input
-- and returns processed output

getListBusinessesR :: Handler (Value)
getListBusinessesR =  do
  page_number' <- lookupGetParams "page[number]"
  page_size'   <- lookupGetParams "page[size]"
  host'        <- lookupHeader "Host" 
  let host = decodeUtf8 (fromMaybe host_default host') 

  pages <- getPage (page_number page_number') (page_size page_size')
  meta  <- getMeta (page_number page_number') (page_size page_size') host 
  return $ object [("businesses" ,toJSON pages),("meta", toJSON meta)]
  where
    host_default = "localhost"
    page_number pn = case pn of
      []      -> error page_number_err
      (pn':_) -> fromText pn'
    page_size ps   = case ps of
      []      -> 50 :: Int
      (ps':_) -> fromText ps'
    page_number_err = 
     "You need to pass a page number as GET parameter." ++ 
     "For example: page[number]=3"

-- | getPage 
--   database query retriving list of usinesses value    
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

-- | getMeta
--   DB query for LinkMap
getMeta :: PageNumber -> PageSize -> Host -> Handler LinkMap
getMeta page_number' page_size' host = do
  meta <- runDB $ do select_range
  case meta of
    [] -> notFound 
    (fp:_) -> return $
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

-- | toLinks
-- creates LinkId LinkUrls pairs
-- prev and next are hackey FIXME
toLinks :: KeyVal           ->
           KeyVal           ->
           KeyVal           ->
           PageNumber64     ->
           PageSize64       ->
           Host             ->
           Pagination       ->
           (LinkId,LinkUrls)
toLinks lb_key ub_key final_key page_number' page_size' host (Pagination self _ prev next _) = 
  (LinkId sid,(LinkUrls self' first' prev' next' last'))
  where
    sid = fromSqlKey self
    self'  = link (show page_number') 
    first' = link (show (1 :: Int)) 
    prev'  =
      let pn = case prev of
           Just num -> if (fromSqlKey num) < lb_key
                       then show (page_number' - 1)
                       else show page_number'
           Nothing  -> "null"
      in link pn 
    next'  = 
      let pn = case next of
           Just num -> if (fromSqlKey num) > ub_key
                       then show (page_number' + 1)
                       else show page_number'
           Nothing  -> "null"
      in link pn
    last' =
      let pn = show (final_key `div` page_size' + (final_key `mod` page_size'))
      in link pn
    link pn = 
      "http://"                   ++ 
      host                        ++
      "/businesses?page[number]=" ++ 
      (pack pn)                   ++
      "&page[size]="              ++
      (pack $ show page_size')
-- goes in own module
finalKey :: Entity Pagination -> Int64
finalKey fp = 
  fromSqlKey (fromJustNote page_last_err $ paginationLast (entityVal fp))
  where
    page_last_err = "404 final key not found"

fromText :: Text -> Int
fromText str_int = toInteger :: Int
  where
    toInteger =
      case (readEither (unpack str_int) :: Either String Int) of
        (Left err_msg) -> error ("Hard Fail key value : " ++ " " ++ (unpack str_int) ++ err_msg)
        (Right int)    -> int


