module Handler.ListBusinessesSpec (spec) where

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
  describe "valid request" $ do
    it "gives a 200" $ do
      request $ do
        addGetParam "page[number]" "3"
        addGetParam "page[size]" "1"
        setUrl ListBusinessesR
      (Just test) <- getResponse
      let decode_test = (decode $ simpleBody test) :: Maybe Value
      liftIO $ print (simpleBody test)
      statusIs 200
  describe "correct page size default" $ do 
    it "Evaluates to True" $ do
      request $ do
        addGetParam "page[number]" "3"
        setUrl ListBusinessesR
      resp <- getResponse
      assertEqual "Default Page Size is 50" (pageSize resp) 50
  describe "correct error when missing required parameter" $ do
    it "Evaluates to True" $ do
     request $ do
       setUrl ListBusinessesR
     resp <- getResponse
     case resp of
       (Just resp) ->
         assertEqual "Output should be custom error" (simpleBody resp) internal_error_resp
       Nothing     -> error ("Hard Fail: Failed to get response with empty paramters ListBuisnessesR")
  describe "404 when passed out of bounds input" $ do
    it "Evaluates to True" $ do
      request $ do
        addGetParam "page[number]" "500000"
        setUrl ListBusinessesR
      resp <- getResponse
      statusIs 404

-- | Here Be Dragons
-- There's a better way, but this way works
pageSize :: Maybe SResponse -> Int
pageSize Nothing = error ("Hard Fail: Response Body Empty")
pageSize (Just resp) = 
  length page_list
  where
    json = case ((decode $ simpleBody resp) :: Maybe Value) of
      Nothing -> error ("Hard Fail: Failed to decode json in pageSize")
      (Just json') -> json' 
    hashmap = case (fromJSON json :: Result Object) of
      (Error err)     -> error ("Hard Fail: " ++ err)
      (Success hashmap') -> hashmap'
    (_:((_,value):_)) = toList hashmap
    page_list = case (fromJSON value :: Result Array) of
      (Error err) -> error ("Hard Fail: " ++ err)
      (Success page_list') -> page_list'

out_of_bounds_resp = 
  "<!DOCTYPE html>\n<html><head><title>Internal Server Error</title></head><body><h1>Internal Server Error</h1>\n<pre>You gave a value that is out of range - keep it in bounds</pre>\n</body></html>"

internal_error_resp =
  "<!DOCTYPE html>\n<html><head><title>Internal Server Error</title></head><body><h1>Internal Server Error</h1>\n<pre>You need to pass a page number as GET parameter.For example: page[number]=3</pre>\n</body></html>"
