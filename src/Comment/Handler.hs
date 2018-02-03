{-# LANGUAGE OverloadedStrings #-}

module Comment.Handler
  (
    createHandler
  , removeListHandler
  , removeHandler
  , getListHandler
  ) where

import           Control.Monad           (void)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (lift)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Lazy    as LB (ByteString, empty)
import           Data.GraphQL            (graphql)
import           Network.HTTP.Types      (status204)
import           Web.Scotty.Trans        (body, json, param, raw, rescue,
                                          status)

import qualified Comment.API             as API
import           Comment.Types           (ListQuery (..))
import           Data.Maybe              (fromMaybe)
import           Data.UnixTime

import           Yuntan.Types.ListResult (From, ListResult (..), Size)
import           Yuntan.Types.OrderBy    (desc)
import           Yuntan.Types.Scotty     (ActionH)
import           Yuntan.Utils.Scotty     (errBadRequest, ok, okListResult)

import           Data.Int                (Int64)
import           Data.Text               (Text)
import           Haxl.Core               (GenHaxl)
import           Yuntan.Types.HasMySQL   (HasMySQL)

resultOK :: ActionH u ()
resultOK = ok "result" ("OK" :: String)

createHandler :: HasMySQL u => ActionH u ()
createHandler = do
  for <- param "for"
  who <- param "who"
  comment <- param "comment"
  cid <- lift $ API.create for who comment
  json =<< lift (API.get cid)

removeHandler :: HasMySQL u => ActionH u ()
removeHandler = do
  cid <- param "id"
  void . lift $ API.remove cid
  resultOK

removeListHandler :: HasMySQL u => ActionH u ()
removeListHandler = do
  for <- param "for"
  void . lift $ API.removeList (LQ1 for)
  resultOK

getListHandler :: HasMySQL u => ActionH u ()
getListHandler = do
  for <- param "for"
  from <- param "from" `rescue` (\_ -> return (0::From))
  size <- param "size" `rescue` (\_ -> return (10::Size))
  comments <- lift $ API.getList (LQ1 for) from size (desc "id")
  total <- lift $ API.count (LQ1 for)
  okListResult "comments" ListResult
    { getFrom   = from
    , getSize   = size
    , getTotal  = total
    , getResult = comments
    }
