{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Comment.DataSource (
    CommentReq(..),
    initCommentState
  ) where

import           Data.Hashable              (Hashable (..))
import           Data.Typeable              (Typeable)
import           Haxl.Core                  (BlockedFetch (..), DataSource,
                                             DataSourceName, Flags,
                                             PerformFetch (..), ShowP, State,
                                             StateKey, dataSourceName, fetch,
                                             putFailure, putSuccess, showp)

import           Comment.DataSource.Comment
import           Comment.DataSource.Table
import           Comment.Types
import           Yuntan.Types.HasMySQL      (HasMySQL, MySQL, mysqlPool,
                                             tablePrefix)

import qualified Control.Exception          (SomeException, bracket_, try)
import           Data.Int                   (Int64)
import           Data.Pool                  (withResource)

import           Data.Text                  (Text)
import           Yuntan.Types.ListResult    (From, Size)
import           Yuntan.Types.OrderBy       (OrderBy)

import           Control.Concurrent.Async
import           Control.Concurrent.QSem

-- Data source implementation.

data CommentReq a where
  MergeData  :: CommentReq ()
  Create     :: Text -> Text -> Text -> CommentReq Int64
  GetList    :: ListQuery -> From -> Size -> OrderBy -> CommentReq [Comment]
  Count      :: ListQuery -> CommentReq Int64
  Remove     :: Int64 -> CommentReq ()
  RemoveList :: ListQuery -> CommentReq ()
  Get        :: Int64 -> CommentReq (Maybe Comment)

  deriving (Typeable)

deriving instance Eq (CommentReq a)
instance Hashable (CommentReq a) where
  hashWithSalt s MergeData         = hashWithSalt s (0::Int)
  hashWithSalt s (Create a b c)    = hashWithSalt s (1::Int, a, b, c)
  hashWithSalt s (GetList a b c d) = hashWithSalt s (2::Int, a, b, c, d)
  hashWithSalt s (Count a)         = hashWithSalt s (3::Int, a)
  hashWithSalt s (Remove a)        = hashWithSalt s (4::Int, a)
  hashWithSalt s (RemoveList a)    = hashWithSalt s (5::Int, a)
  hashWithSalt s (Get a)           = hashWithSalt s (6::Int, a)

deriving instance Show (CommentReq a)
instance ShowP CommentReq where showp = show

instance StateKey CommentReq where
  data State CommentReq = CommentState { numThreads :: Int }

instance DataSourceName CommentReq where
  dataSourceName _ = "CommentDataSource"

instance HasMySQL u => DataSource u CommentReq where
  fetch = doFetch

doFetch
  :: HasMySQL u
  => State CommentReq
  -> Flags
  -> u
  -> PerformFetch CommentReq

doFetch _state _flags _user = AsyncFetch $ \reqs inner -> do
  sem <- newQSem $ numThreads _state
  asyncs <- mapM (fetchAsync sem _user) reqs
  inner
  mapM_ wait asyncs

fetchAsync :: HasMySQL u => QSem -> u -> BlockedFetch CommentReq -> IO (Async ())
fetchAsync sem env req = async $
  Control.Exception.bracket_ (waitQSem sem) (signalQSem sem) $ withResource pool $ fetchSync req prefix

  where pool   = mysqlPool env
        prefix = tablePrefix env

fetchSync :: BlockedFetch CommentReq -> MySQL ()
fetchSync (BlockedFetch req rvar) prefix conn = do
  e <- Control.Exception.try $ fetchReq req prefix conn
  case e of
    Left ex -> putFailure rvar (ex :: Control.Exception.SomeException)
    Right a -> putSuccess rvar a

fetchReq :: CommentReq a -> MySQL a
fetchReq MergeData         = mergeData
fetchReq (Create a b c)    = create a b c
fetchReq (GetList a b c d) = getList a b c d
fetchReq (Count a)         = count a
fetchReq (Remove a)        = remove a
fetchReq (RemoveList a)    = removeList a
fetchReq (Get a)           = get a

initCommentState :: Int -> State CommentReq
initCommentState = CommentState
