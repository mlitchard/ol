{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards, DeriveGeneric, OverloadedStrings, ViewPatterns, ScopedTypeVariables #-}
module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import Control.Monad.Logger                 (liftLoc, runLoggingT, NoLoggingT)
import Database.Persist.Postgresql          ( withPostgresqlPool
                                            , runSqlPersistMPool
                                            , createPostgresqlPool
                                            , pgConnStr, pgPoolSize, runSqlPool)
import Database.Persist.Sql (toSqlKey,fromSqlKey)
import Import hiding (unlines,unpack,pack,first,toInteger) 
import Data.Text hiding (lines,length) -- (splitOn)
import Text.Read (readEither)
import qualified Data.Vector as Vector

import qualified Data.Csv as CSV
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as UTF8
-- import System.IO (readFile)
-- import Data.List.Split (splitOn)
import Language.Haskell.TH.Syntax           (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)

import Control.Monad.Logger (runStdoutLoggingT)
-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.ListBusinesses
import Handler.Business

type FirstKey    = (Key Businesses)
type BusinessKey = (Key Businesses)
type PrevKey     = Maybe (Key Businesses)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

instance CSV.FromRecord Businesses
instance CSV.ToRecord Businesses
-- instance FromNamedRecord Businesses where
--    parseNamedRecord r = Person <$> r .: "" <*> r .: "salary"
-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc
    popDB appSettings
    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
--

popDB :: AppSettings -> IO ()
popDB appSettings = do
 first <- newEmptyMVar :: IO (MVar FirstKey)
 prev <- newEmptyMVar :: IO (MVar PrevKey)

 (_:businesses) <- lines <$> readFile "engineering_project_businesses.csv"
 runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> 
   liftIO $ flip runSqlPersistMPool pool $ do
     mapM_ (processFile first prev) businesses
--     mapM_ (processFile (length $ unpack businesses)) businesses 
     return ()
  where
    connStr = pgConnStr $ appDatabaseConf appSettings

processFile :: MVar FirstKey -> 
               MVar PrevKey  ->
               Text          -> 
               ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
processFile first prev b_record' = do
  liftIO $ putStrLn ("b_record is " ++ (b_record))
  first_empty <- isEmptyMVar first
  repsert (business_key) business 
  _ <- case first_empty of
         True  -> do
                   liftIO $ putStrLn ("FIRST IS EMPTY") 
                   putMVar first business_key 
                   putMVar prev  (Just business_key)
                   _ <- repsert pagination_key $ makePaginationFirst
                   return ()
         False -> do
                   liftIO $ putStrLn ("FIRST SHOULD NOT BE EMPTY")
                   first_key <- readMVar first
                   liftIO $ putStrLn ("id is " ++ id' ++ "first key is " ++ (pack (show first_key)))
                   prev_key  <- readMVar prev
                   liftIO $ putStrLn ("prev_key is " ++ (pack (show prev_key)))
                   popNextField prev_key business_key
                   _ <- repsert pagination_key $ makePagination first_key prev_key
                   -- ^ populate the next field of the previous entry
--                   popPrevField prev_key business_key
                   -- ^ populates the prev field of currrent entry
                   swapMVar prev (Just business_key)
                   return ()
  liftIO $ putStrLn ("RESERT COMPLETED")
  return ()
  where
    (business:_) = makeBRecord (BS.fromStrict (UTF8.fromString (unpack b_record)))
    (id',b_record_wcomma) = breakOn "," b_record'
    b_record = Data.Text.drop 1 b_record_wcomma
    business_key   = toSqlKey (keyValue id') :: Key Businesses
    pagination_key = toSqlKey (keyValue id') :: Key Pagination
    makePagination first_key prev_key = 
      Pagination business_key first_key prev_key Nothing Nothing 
    makePaginationFirst = 
      Pagination business_key business_key Nothing Nothing Nothing
--    madd2 = case add2 of
--      "" -> Nothing
--      _  -> Just add2

makeBRecord :: BS.ByteString -> [Businesses]
makeBRecord b_record = 
  case (CSV.decode CSV.NoHeader b_record :: Either String (Vector Businesses)) of
    Left err_msg -> error err_msg
    Right b_record -> Vector.toList b_record
  
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

popPrevField :: PrevKey     ->
                BusinessKey ->
                ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
popPrevField Nothing current_key = error err_msg
  where
    current_id = show (fromSqlKey current_key)
    err_msg =
      "Hard Fail: " ++ current_id ++
      " failed to populate current record's previous field -- MVar empty"
popPrevField (Just prev_key) current_key = do
  maybePaginateKey <- getBy $ UniqueBusinessesId current_key
  case (maybePaginateKey) of
    Nothing -> error not_found
    Just (Entity current_key' _) -> 
      update current_key' [PaginationPrev =. (Just prev_key)]
  where
    key_value = show (fromSqlKey current_key)
    not_found = 
      "Hard Fail: " ++ key_value                ++ 
      " failed to be found in Pagination table" ++
      " while trying to populate previous field"

    
    
    
    
keyValue :: Text -> Int64
keyValue str_int = fromIntegral toInteger :: Int64
  where
    toInteger =
      case (readEither (unpack str_int) :: Either String Integer) of
        (Left err_msg) -> error ("Hard Fail key value : " ++ " " ++ (unpack str_int) ++ err_msg)
        (Right int)    -> int
  
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB
