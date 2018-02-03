{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Comment.Types
  (
    Comment (..)
  , ListQuery (..)
  , lq2A
  , lq2T
  ) where

import           Database.MySQL.Simple              (Only (..))
import           Database.MySQL.Simple.Param        (Action)
import           Database.MySQL.Simple.QueryParams  (renderParams)
import           Database.MySQL.Simple.QueryResults (QueryResults, convertError,
                                                     convertResults)
import           Database.MySQL.Simple.Result       (convert)

import           Data.Aeson                         (ToJSON (..), Value (Null),
                                                     decodeStrict, object, (.=))

import           Data.Int                           (Int64)
import           Data.Maybe                         (fromMaybe)
import           Data.Text                          (Text)

import           Data.Hashable                      (Hashable (..))
import           GHC.Generics                       (Generic)

data Comment = Comment
  { commentId        :: Int64
  , commentFor       :: Text
  , commentWho       :: Text
  , commentText      :: Text
  , commentExtra     :: Value
  , commentCreatedAt :: Int64
  }
  deriving (Show)

instance QueryResults Comment where
  convertResults [fa, fb, fc, fd, _ , ff]
                 [va, vb, vc, vd, ve, vf] = Comment{..}
    where !commentId        = convert fa va
          !commentFor       = convert fb vb
          !commentWho       = convert fc vc
          !commentText      = convert fd vd
          !commentExtra     = fromMaybe Null . decodeStrict $ fromMaybe "{}" ve
          !commentCreatedAt = convert ff vf
  convertResults fs vs  = convertError fs vs 2

instance ToJSON Comment where
  toJSON Comment{..} = object
    [ "id"         .= commentId
    , "for"        .= commentFor
    , "who"        .= commentWho
    , "text"       .= commentText
    , "extra"      .= commentExtra
    , "created_at" .= commentCreatedAt
    ]

data ListQuery = LQ1 Text
               | LQ2 Text

  deriving (Generic, Eq, Show)

instance Hashable ListQuery

fieldT :: String -> String
fieldT n = "`" ++ n ++ "` = ?"

lq2T :: ListQuery -> String
lq2T LQ1{} = fieldT "for"
lq2T LQ2{} = fieldT "who"

lq2A :: ListQuery -> [Action]
lq2A (LQ1 n) = renderParams (Only n)
lq2A (LQ2 n) = renderParams (Only n)
