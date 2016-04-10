{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Database.Models where

import Data.Aeson                  (ToJSON, FromJSON)
import GHC.Generics                (Generic)
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Database.Persist.Sql
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)
import Data.List.NonEmpty ( NonEmpty(..) )

import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
CountryRow
    name String
    altNames [String]
    capital String
    capitalNames [String]
    deriving Show
|]

data Country = Country
  { countryName    :: NonEmpty String
  , countryCapital :: NonEmpty String
  } deriving (Eq, Show, Generic)

instance ToJSON Country
instance FromJSON Country

countryRowToCountry :: CountryRow -> Country
countryRowToCountry CountryRow{..} =
  Country
  { countryName = countryRowName :| countryRowAltNames
  , countryCapital = countryRowCapital :| countryRowCapitalNames
  }

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool