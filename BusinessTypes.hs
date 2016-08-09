{-# LANGUAGE DeriveGeneric #-}
module BusinessTypes 
  ( FirstKey
  , BusinessKey
  , PrevKey
  , DatabaseReaderT
  , LinkMap
  , PageNumber
  , PageSize
  , PageNumber64
  , PageSize64
  , KeyVal
  , Host
  , LinkId (..) 
  , LinkUrls (..)
  ) where

import Control.Monad.Logger                 (NoLoggingT)

import Import 
type FirstKey    = (Key Businesses)
type BusinessKey = (Key Businesses)
type PrevKey     = Maybe (Key Businesses)

type DatabaseReaderT = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()

type LinkMap = [(LinkId, LinkUrls)]
type PageNumber = Int
type PageSize   = Int
type PageNumber64 = Int64
type PageSize64   = Int64
type KeyVal       = Int64
type Host       = Text
newtype LinkId = LinkId {linkID :: Int64} deriving (Show,Eq,Ord,Generic,Hashable)

data LinkUrls = LinkUrls
  { self  :: Text
  , first :: Text
  , prev  :: Text
  , next  :: Text
  , last  :: Text
  } deriving (Generic,Show)

instance ToJSON LinkId
instance FromJSON LinkId
instance ToJSON LinkUrls
instance FromJSON LinkUrls
