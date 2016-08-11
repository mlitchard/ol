{-# LANGUAGE OverloadedStrings #-}
module Handler.ListBusinessesSpec (spec) where

-- http://localhost:3000/businesses' -d "page[number]=3"
-- addGetParam :: Text -> Text -> RequestBuilder site ()
import TestImport hiding (fromList,toList)
import Data.HashMap.Strict
import Network.Wai.Test hiding (request)
import Data.Aeson hiding (json')

spec :: Spec
spec = withApp $ do
  describe "valid request" $ do
    it "gives a 200" $ do
      request $ do
        addGetParam "page[number]" "3"
        addGetParam "page[size]" "1"
        setUrl ListBusinessesR
      _ <- getResponse
      statusIs 200
  describe "correct page size default" $ do 
    it "Evaluates to True" $ do
      request $ do
        addGetParam "page[number]" "3"
        setUrl ListBusinessesR
      resp <- getResponse
      assertEqual "Default Page Size is 50" (pageSize resp) 50
  describe "test edge case" $ do
    it "Evaluates to True" $ do
      request $ do
        addGetParam "page[number]" "1"
        addGetParam "page[size]" "1"
        setUrl ListBusinessesR
      resp <- getResponse
      case resp of
        Just resp' -> assertEqual "edge case is correct" (simpleBody resp') edge_case_resp
        Nothing    -> error ("Hard Fail: empty response when testing edge case")
  describe "correct error when missing required parameter" $ do
    it "Evaluates to True" $ do
     request $ do
       setUrl ListBusinessesR
     resp <- getResponse
     case resp of
       (Just resp') ->
         assertEqual "Output should be custom error" (simpleBody resp') internal_error_resp
       Nothing     -> error ("Hard Fail: Failed to get response with empty paramters ListBuisnessesR")
  describe "404 when passed out of bounds input" $ do
    it "Evaluates to True" $ do
      request $ do
        addGetParam "page[number]" "500000"
        setUrl ListBusinessesR
      _ <- getResponse
      statusIs 404

-- | Here Be Dragons
-- There's a better way, but this way works
pageSize :: Maybe SResponse -> Int
pageSize Nothing = error ("Hard Fail: Response Body Empty")
pageSize (Just resp) = 
  length page_list
  where
    json_ = case ((decode $ simpleBody resp) :: Maybe Value) of
      Nothing -> error ("Hard Fail: Failed to decode json in pageSize")
      (Just json') -> json' 
    hashmap = case (fromJSON json_ :: Result Object) of
      (Error err)     -> error ("Hard Fail: " ++ err)
      (Success hashmap') -> hashmap'
    (_:((_,value):_)) = toList hashmap
    page_list = case (fromJSON value :: Result Array) of
      (Error err) -> error ("Hard Fail: " ++ err)
      (Success page_list') -> page_list'

edge_case_resp =
  "{\"meta\":[[{\"linkID\":0},{\"next\":\"http://localhost/businesses?page[number]=2&page[size]=1\",\"first\":\"http://localhost/businesses?page[number]=1&page[size]=1\",\"self\":\"http://localhost/businesses?page[number]=1&page[size]=1\",\"last\":\"http://localhost/businesses?page[number]=49999&page[size]=1\",\"prev\":\"http://localhost/businesses?page[number]=null&page[size]=1\"}]],\"businesses\":[{\"address2\":\"Suite 517\",\"phone\":\"4034880719\",\"state\":\"IA\",\"id'\":0,\"uuid\":\"2859d6e0-1cb9-4fe9-bc00-97823a9fa4cb\",\"country\":\"US\",\"zip\":\"19416\",\"website\":\"http://www.halvorson.com/\",\"address\":\"1386 Lim Brooks\",\"name\":\"Yundt-Flatley\",\"city\":\"Lake Betsy\",\"created_at\":\"2012-12-10 16:17:58\"}]}"
out_of_bounds_resp = 
  "<!DOCTYPE html>\n<html><head><title>Internal Server Error</title></head><body><h1>Internal Server Error</h1>\n<pre>You gave a value that is out of range - keep it in bounds</pre>\n</body></html>"

internal_error_resp =
  "<!DOCTYPE html>\n<html><head><title>Internal Server Error</title></head><body><h1>Internal Server Error</h1>\n<pre>You need to pass a page number as GET parameter.For example: page[number]=3</pre>\n</body></html>"
