{-# LANGUAGE OverloadedStrings #-}

module Comment.DataSource.Comment
  (
    create
  , get
  , getList
  , count
  , remove
  , removeList
  ) where

import           Comment.Types
import           Control.Monad                     (void)
import           Data.Int                          (Int64)
import           Data.Maybe                        (listToMaybe)
import           Data.String                       (fromString)
import           Data.Text                         (Text)
import           Data.UnixTime
import           Database.MySQL.Simple             (Only (..), execute,
                                                    insertID, query)
import           Database.MySQL.Simple.QueryParams (renderParams)
import           Yuntan.Types.HasMySQL             (MySQL)
import           Yuntan.Types.ListResult           (From, Size)
import           Yuntan.Types.OrderBy              (OrderBy)

create :: Text -> Text -> Text -> MySQL Int64
create for who text prefix conn = do
  t <- show . toEpochTime <$> getUnixTime
  void . flip (execute conn) (for, who, text, t) . fromString $ concat
    [ "INSERT INTO `", prefix, "_comments` (`for`, `who`, `comment`, `created_at`)"
    , " VALUES (?, ?, ?, ?)"
    ]

  fromIntegral <$> insertID conn

getList :: ListQuery -> From -> Size -> OrderBy -> MySQL [Comment]
getList lq from size o prefix conn =
  flip (query conn) (lq2A lq ++ renderParams (from, size)) . fromString $ concat
    [ "SELECT * FROM `", prefix, "_comments`"
    , " WHERE ", lq2T lq
    , " ", show o, " LIMIT ?,?"
    ]

count :: ListQuery -> MySQL Int64
count lq prefix conn =
  maybe 0 fromOnly . listToMaybe
    <$> query conn (fromString $ concat
      [ "SELECT count(*) FROM `", prefix, "_comments` WHERE ", lq2T lq ]) (lq2A lq)


remove :: Int64 -> MySQL ()
remove cid prefix conn =
  void . flip (execute conn) (Only cid) . fromString $ concat
    [ "DELETE FROM `", prefix, "_comments` WHERE `id`=?" ]

removeList :: ListQuery -> MySQL ()
removeList lq prefix conn =
  void . flip (execute conn) (lq2A lq) . fromString $ concat
    [ "DELETE FROM `", prefix, "_comments` WHERE ", lq2T lq]

get :: Int64 -> MySQL (Maybe Comment)
get cid prefix conn =
  listToMaybe <$> query conn (fromString $ concat
    [ "SELECT * FROM `", prefix, "_comments` WHERE `id`=?" ]) (Only cid)
