{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Import hiding (unlines,unpack,pack) 
import Data.Text hiding (lines,length) -- (splitOn)
import Text.Read (readEither)
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

data AppKey = Business | Paginate deriving Show
-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

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
 first <- newEmptyMVar :: IO (MVar (Key Businesses))
 prev  <- newEmptyMVar :: IO (MVar (Key Businesses))

 (_:b1:businesses) <- lines <$> readFile "engineering_project_businesses.csv"
 runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> 
   liftIO $ flip runSqlPersistMPool pool $ do
     processFile prev first b1
--     mapM_ (processFile (length $ unpack businesses)) businesses 
     return ()
-- where
processFile :: MVar (Key Businesses) -> 
               MVar (Key Businesses) ->
               Text                  -> 
               ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
processFile prev first line = do
  first_empty <- isEmptyMVar first
  repsert (key Business) $ 
    Businesses uuid name add madd2 city state zip' country phone website (unlines ca) 
  _ <- case first_empty of
         True  -> do 
                   putMVar first 
                   putMVar prev (key
                   _ <- repsert (makeKey id') $ Pagination (makeKey id') (makeKey id') Nothing Nothing Nothing
                   return ()
         False -> do
                   first' <- readMVar first
                   prev'  <- readMVar prev
                      
                   return ()
  where
    key = makeKey (keyValue id')
     
     
--     let key@((PersistInt64 key_val):_) = keyToValues bid
--     let new_id = keyFromValues key :: Either Text (Key Businesses)
--     putStrLn ("file size is " ++ (pack $ show file_size))
--     putStrLn ("key is " ++ (pack $ show key_val))
     return ()
 
{-  
     putStrLn ("uuid is " ++ uuid)
     putStrLn ("name is " ++ name)
     putStrLn ("add is "  ++ add)
     putStrLn ("add2 is " ++ add2)
     putStrLn ("city is " ++ city)
     putStrLn ("state is " ++ state)
     putStrLn ("zip is " ++ zip)
     putStrLn ("country is " ++ country)
     putStrLn ("phone is " ++ phone)
     putStrLn ("website is " ++ website)
     putStrLn ("ca is " ++ (Import.unlines ca))
-}
     where
       (id':uuid:name:add:add2:city:state:zip':country:phone:website:ca) = 
         splitOn "," line
      
       madd2 = case add2 of
         "" -> Nothing
         _  -> Just add2
   connStr = pgConnStr $ appDatabaseConf appSettings

makeKey :: [PersistValue Int64] ->
           AppKey               ->
           Either (Key Paginator) (Key Businesses)
makeKey key_value Business = key
  where
    key       = 
      case keyFromValues key_value :: Either Text (Key Businesses) of
        (Left err) -> 
          error ("Hard Fail: Failed to make " ++ (unpack err) ++ "into key")
        (Right key') -> key'
makeKey key_value Paginate = key
  where
    key       =
      case keyFromValues key_value :: Either Text (Key Paginator) of
        (Left err) ->
          error ("Hard Fail: Failed to make " ++ (unpack err) ++ "into key")
        (Right key') -> key'

keyValue :: Text -> [PersistValue Int64]
keyValue str_int = [toPersistValue (fromIntegral toInteger :: Int64)]
  where
    toInteger =
      case (readEither (unpack str_int) :: Either String Integer) of
        (Left err_msg) -> error ("Hard Fail : " ++ err_msg)
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
