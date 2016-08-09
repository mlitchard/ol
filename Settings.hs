{-# Language CPP #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import ClassyPrelude.Yesod
import Control.Exception           (throw)
import Data.Aeson                  (withObject, (.!=),
                                    (.:?))
import Data.FileEmbed              (embedFile)
import Data.Yaml                   (decodeEither')
import Database.Persist.Postgresql (PostgresConf)
import Network.Wai.Handler.Warp    (HostPreference)
import Yesod.Default.Config2       (configSettingsYml)

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { 
      appDatabaseConf           :: PostgresConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev = False
-- #if DEVELOPMENT
--                True
-- #else
--                False
-- #endif
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        appDetailedRequestLogging <- o .:? 
          "detailed-logging" .!= False
        appShouldLogAll           <- o .:? 
          "should-log-all"   .!= False
        return AppSettings {..}

-- The rest of this file contains settings which rarely need changing by a
-- user.

-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue = 
   either throw id $ decodeEither' configSettingsYmlBS

-- | A version of @AppSettings@ parsed at compile time from @config/settings.yml@.
-- compileTimeAppSettings :: AppSettings
-- compileTimeAppSettings =
--    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
--        Error e -> error e
--        Success settings -> settings

