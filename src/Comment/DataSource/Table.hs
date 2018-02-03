{-# LANGUAGE OverloadedStrings #-}

module Comment.DataSource.Table
  (
    mergeData
  ) where

import           Database.MySQL.Simple (execute_)

import           Control.Monad         (void)
import           Data.String           (fromString)
import           Yuntan.Types.HasMySQL (MySQL, VersionList, mergeDatabase)

createCommentTable :: MySQL ()
createCommentTable prefix conn = void . execute_ conn . fromString $ concat
  [ "CREATE TABLE IF NOT EXISTS `", prefix, "_comments` ("
  , "  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,"
  , "  `for` varchar(128) NOT NULL,"
  , "  `who` varchar(128) DEFAULT '',"
  , "  `comment` varchar(1500) NOT NULL,"
  , "  `created_at` int(10) unsigned NOT NULL,"
  , "  PRIMARY KEY (`id`),"
  , "  KEY `for` (`for`),"
  , "  KEY `who` (`who`)"
  , ") ENGINE=InnoDB DEFAULT CHARSET=utf8"
  ]

versionList :: VersionList
versionList =
  [ (1, [createCommentTable])
  ]

mergeData :: MySQL ()
mergeData = mergeDatabase versionList
