{-# LANGUAGE OverloadedStrings #-}

module Comment.Handler
  (
    createHandler
  , removeListHandler
  , removeHandler
  , getListHandler
  , getHandler
  ) where

import           Control.Monad           (void)
import           Control.Monad.Reader    (lift)
import           Web.Scotty.Trans        (json, param, rescue)

import qualified Comment.API             as API
import           Comment.Types           (ListQuery (..))

import           Yuntan.Types.ListResult (From, ListResult (..), Size)
import           Yuntan.Types.OrderBy    (desc)
import           Yuntan.Types.Scotty     (ActionH)
import           Yuntan.Utils.Scotty     (ok, okListResult)

import           Yuntan.Types.HasMySQL   (HasMySQL)

resultOK :: ActionH u w ()
resultOK = ok "result" ("OK" :: String)

createHandler :: HasMySQL u => ActionH u w ()
createHandler = do
  for <- param "for"
  who <- param "who"
  comment <- param "comment"
  cid <- lift $ API.create for who comment
  json =<< lift (API.get cid)

removeHandler :: HasMySQL u => ActionH u w ()
removeHandler = do
  cid <- param "id"
  void . lift $ API.remove cid
  resultOK

getHandler :: HasMySQL u => ActionH u w ()
getHandler = do
  cid <- param "id"
  json =<< lift (API.get cid)

removeListHandler :: HasMySQL u => ActionH u w ()
removeListHandler = do
  for <- param "for"
  void . lift $ API.removeList (LQ1 for)
  resultOK

getListHandler :: HasMySQL u => ActionH u w ()
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
