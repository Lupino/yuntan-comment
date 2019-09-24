module Comment.API
  (
    create
  , get
  , getList
  , count
  , remove
  , removeList

  , mergeData
  ) where

import           Data.Int                (Int64)
import           Haxl.Core               (GenHaxl, dataFetch, uncachedRequest)

import           Comment.DataSource
import           Comment.Types
import           Data.Text               (Text)
import           Yuntan.Types.HasMySQL   (HasMySQL)
import           Yuntan.Types.ListResult (From, Size)
import           Yuntan.Types.OrderBy    (OrderBy)

mergeData  :: HasMySQL u => GenHaxl u w ()
create     :: HasMySQL u => Text -> Text -> Text -> GenHaxl u w Int64
get        :: HasMySQL u => Int64 -> GenHaxl u w (Maybe Comment)
getList    :: HasMySQL u => ListQuery -> From -> Size -> OrderBy -> GenHaxl u w [Comment]
count      :: HasMySQL u => ListQuery -> GenHaxl u w Int64
remove     :: HasMySQL u => Int64 -> GenHaxl u w ()
removeList :: HasMySQL u => ListQuery -> GenHaxl u w ()

mergeData       = uncachedRequest MergeData
create a b c    = uncachedRequest (Create a b c)
getList a b c d = dataFetch (GetList a b c d)
count a         = dataFetch (Count a)
get a           = dataFetch (Get a)
remove a        = uncachedRequest (Remove a)
removeList a    = uncachedRequest (RemoveList a)
