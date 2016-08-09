module Handler.BusinessSpec (spec) where

-- http://localhost:3000/businesses' -d "page[number]=3"
-- addGetParam :: Text -> Text -> RequestBuilder site ()
import TestImport hiding (fromList,toList)
import Data.HashMap.Strict
import qualified Data.Vector as Vector
import Network.Wai.Test hiding (request)
import Data.Aeson
import Data.Maybe
import BusinessTypes

spec :: Spec
spec = withApp $ do
  describe "valid request for single entry" $ do
    it "gives a 200" $ do
      request $ do
        setUrl ("http://localhost:3000/businesses/49999" :: Text)
      statusIs 200
  describe "Gives correct entry when requested" $ do 
    it "Evaluates to True" $ do
      request $ do
        setUrl ("http://localhost:3000/businesses/49999" :: Text)
      resp <- getResponse
      case resp of
        (Just resp) -> assertEqual "Last Entry" (simpleBody resp) last_entry
        Nothing     -> error "Hard Fail: Could not get last entry"
  describe "Gives 404 when attempting to get non-existent entry" $ do
    it "Evaluates to True" $ do
      request $ do
        setUrl ("http://localhost:3000/businesses/50000" :: Text)
      statusIs 404

last_entry =
  "{\"next\":null,\"first\":0,\"self\":49999,\"last\":49999,\"id\":49999,\"prev\":49998}"
