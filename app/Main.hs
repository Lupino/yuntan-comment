{-# LANGUAGE OverloadedStrings #-}

module Main
  (
    main
  ) where

import           Data.Default.Class                   (def)
import           Data.Streaming.Network.Internal      (HostPreference (Host))
import           Network.Wai.Handler.Warp             (setHost, setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans                     (delete, get, middleware,
                                                       post, scottyOptsT,
                                                       settings)

import           Comment.API                          (mergeData)
import           Comment.DataSource                   (initCommentState)
import           Comment.Handler
import           Haxl.Core                            (GenHaxl, StateStore,
                                                       initEnv, runHaxl,
                                                       stateEmpty, stateSet)

import           Yuntan.Types.Scotty                  (ScottyH)

import           Yuntan.Types.HasMySQL                (HasMySQL, simpleEnv)


import qualified Comment.Config                       as C
import qualified Data.Yaml                            as Y

import           Data.Semigroup                       ((<>))
import           Options.Applicative

data Options = Options { getConfigFile  :: String
                       , getHost        :: String
                       , getPort        :: Int
                       , getTablePrefix :: String
                       }

parser :: Parser Options
parser = Options <$> strOption (long "config"
                                <> short 'c'
                                <> metavar "FILE"
                                <> help "Comment micro server config file."
                                <> value "config.yaml")
                 <*> strOption (long "host"
                                <> short 'H'
                                <> metavar "HOST"
                                <> help "Comment micro server hostname."
                                <> value "127.0.0.1")
                 <*> option auto (long "port"
                                <> short 'p'
                                <> metavar "PORT"
                                <> help "Comment micro server port."
                                <> value 3000)
                 <*> strOption (long "table_prefix"
                                <> metavar "TABLE_PREFIX"
                                <> help "table prefix."
                                <> value "test")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Comment micro server"
     <> header "coin - Comment micro server" )

program :: Options -> IO ()
program Options { getConfigFile = confFile
                , getHost = host
                , getPort = port
                , getTablePrefix = prefix
                } = do
  (Right conf) <- Y.decodeFileEither confFile

  let mysqlConfig  = C.mysqlConfig conf
      mysqlThreads = C.mysqlHaxlNumThreads mysqlConfig

  pool <- C.genMySQLPool mysqlConfig

  let state = stateSet (initCommentState mysqlThreads) stateEmpty

  let opts = def { settings = setPort port
                            $ setHost (Host host) (settings def) }

  runIO (simpleEnv pool prefix ()) state mergeData
  scottyOptsT opts (runIO (simpleEnv pool prefix ()) state) application
  where
        runIO :: HasMySQL u => u -> StateStore -> GenHaxl u b -> IO b
        runIO env s m = do
          env0 <- initEnv s env
          runHaxl env0 m

application :: HasMySQL u => ScottyH u ()
application = do
  middleware logStdout

  get    "/api/comments/:for/" getListHandler
  delete "/api/comments/:for/" removeListHandler
  delete "/api/comment/:id/"   removeHandler
  get    "/api/comment/:id/"   getHandler
  post   "/api/comments/:for/" createHandler
