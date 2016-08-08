module BusinessTypes (FirstKey, BusinessKey, PrevKey, DatabaseReaderT) where

import Control.Monad.Logger                 (NoLoggingT)

import Import 
type FirstKey    = (Key Businesses)
type BusinessKey = (Key Businesses)
type PrevKey     = Maybe (Key Businesses)

type DatabaseReaderT = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
