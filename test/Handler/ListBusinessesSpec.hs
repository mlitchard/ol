module Handler.ListBusinessesSpec (spec) where

-- http://localhost:3000/businesses' -d "page[number]=3"
-- addGetParam :: Text -> Text -> RequestBuilder site ()
import TestImport
spec :: Spec
spec = withApp $ do
  describe "valid request" $ do
    it "gives a 200" $ do
      request $ do
        addGetParam "page[number]" "3"
        setUrl ListBusinessesR
      statusIs 200 
