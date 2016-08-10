module Library.PopulateDB (popDB) where

{- Module: Library.PopulateDb
   Handles initial population of postgres database from
   csv file
-}

-- | Database and Logging functions
import Database.Persist.Postgresql          ( withPostgresqlPool
                                            , runSqlPersistMPool
                                            , pgConnStr) 
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Control.Monad.Logger (NoLoggingT,runNoLoggingT)

-- | All the functions involved in the intermidate data munging
--   from outside modules
import Text.Read (readEither)
import Data.Text hiding (lines,length)
import qualified Data.Csv as CSV ( FromRecord
                                 , ToRecord
                                 , HasHeader (NoHeader)
                                 , decode)
import qualified Data.ByteString.Lazy as BS (ByteString,fromStrict)
import qualified Data.ByteString.UTF8 as UTF8 (fromString)
import qualified Data.Vector as Vector (toList)

-- | Internal
import Import hiding (unlines,unpack,pack,first,toInteger,keys)
import BusinessTypes (FirstKey, BusinessKey, PrevKey, DatabaseReaderT)

-- | popDB - populates the ol database
-- This is a two pass function
-- Pass one uses two MVars to track (1) first id (2) previous id
-- Pass two calculates length of [[Text]] to predict final id,
-- then revisits each db entry and populates "last" field
-- * This function assumes the csv file uses sequential non-negatives *

popDB :: AppSettings -> IO ()
popDB appSettings = do
 first <- newEmptyMVar :: IO (MVar FirstKey)
 prev  <- newEmptyMVar :: IO (MVar PrevKey)

 (_:businesses) <- lines <$> readFile "engineering_project_businesses.csv" :: IO [Text]
 runNoLoggingT $ withPostgresqlPool connStr 10 $ \pool ->
   liftIO $ flip runSqlPersistMPool pool $ do
     keys' <- mapM (processFile first prev) businesses
     mapM_ (processFileLast (final_key keys')) keys'
     return ()
  where
    connStr  = pgConnStr $ appDatabaseConf appSettings

final_key :: [Key Pagination] -> Key Pagination
final_key keys =
  let (last_key:_) = Import.reverse keys -- FIXME - awful awful
  in  last_key

-- | processFile
-- this is the first pass population function
-- takes unprocessed Text and inserts into Database
-- uses MVars to track first and prev fields (and indirectly, next field)
processFile :: MVar FirstKey -> 
               MVar PrevKey  ->
               Text          -> 
               ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Key Pagination)
processFile first prev b_record' = do
  first_empty <- isEmptyMVar first
  repsert (business_key) business 
  _ <- case first_empty of
         True  -> do
                   putMVar first business_key 
                   putMVar prev  (Just business_key)
                   _ <- repsert pagination_key $ make_first_pagination
                   return ()
         False -> do
                   first_key <- readMVar first
                   prev_key  <- readMVar prev
                   _ <- repsert pagination_key $
                        makePagination business_key first_key prev_key
                   popNextField prev_key business_key
                   _ <- swapMVar prev (Just business_key)
                   return ()
  return (pagination_key)
  where
    (business:_) = 
      makeBRecord (BS.fromStrict (UTF8.fromString (unpack b_record')))
    (id',_) = breakOn "," b_record'
    business_key   = toSqlKey (keyValue id') :: Key Businesses
    pagination_key = toSqlKey (keyValue id') :: Key Pagination
    make_first_pagination =
      Pagination business_key business_key Nothing Nothing Nothing 

-- | makePagination
-- it is what it says on the tin
makePagination :: BusinessKey -> FirstKey -> PrevKey -> Pagination
makePagination business_key first_key prev_key =
  Pagination business_key first_key prev_key Nothing Nothing

-- | processFileLast
-- second pass population function
-- This goes through and makes sure each entry has the last field populated 
processFileLast :: Key Pagination -> Key Pagination -> DatabaseReaderT
processFileLast last_key current_key = do
  last_key_pagination' <- last_key_pagination
  update current_key [PaginationLast =. (Just last_key_pagination')]
  return ()
  where
    last_key_pagination = do
      maybe_key <- get last_key
      case (maybe_key) of
        Just (Pagination self _ _ _ _) -> return self
        Nothing                        -> error err_msg
    err_msg =
      "Hard Fail: While trying to populate \"last\" field " ++
      "failed to find " ++ (show last_key)

instance CSV.FromRecord Businesses
instance CSV.ToRecord Businesses

makeBRecord :: BS.ByteString -> [Businesses]
makeBRecord b_record = 
  case (CSV.decode CSV.NoHeader b_record :: Either String (Vector Businesses)) of
    Left err_msg -> error err_msg
    Right b_record' -> Vector.toList b_record'

popNextField :: PrevKey     ->
                BusinessKey ->
                ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
popNextField Nothing current_key = error err_msg
  where
    current_id = show (fromSqlKey current_key)
    err_msg    =
      "Hard Fail: " ++ current_id ++
      " failed to populate previous record's next field -- MVar empty"
popNextField (Just prev_key) current_key = do
  maybePaginateKey <- getBy $ UniqueBusinessesId prev_key
  case (maybePaginateKey) of
    Nothing -> error not_found
    Just (Entity prev_key' _) ->
      update prev_key' [PaginationNext =. (Just current_key)]
  where
    key_value = show (fromSqlKey prev_key)
    not_found =
      "Hard Fail: " ++ key_value                ++
      " failed to be found in Pagination table" ++
      "while trying to populate next field"

-- | keyValue
-- transforms Text into INT64, as Key types need Int64 values
keyValue :: Text -> Int64
keyValue str_int = fromIntegral toInteger :: Int64
  where
    toInteger =
      case (readEither (unpack str_int) :: Either String Integer) of
        (Left err_msg) -> error ("Hard Fail key value : " ++ " " ++ (unpack str_int) ++ err_msg)
        (Right int)    -> int


