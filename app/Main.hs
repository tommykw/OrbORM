{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable, QuasiQuotes #-}
module Main where

import Prelude
import Control.Lens.Action
import Control.Monad (when, void)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Configurator.Types (Config)
import Data.Data (Data)
import Data.Maybe (listToMaybe, isNothing, fromJust)
import Data.Version (showVersion)
import System.Console.CmdArgs.Explicit (HelpFormat(..), helpText)
import Web.Twitter.Conduit (stream, statusesFilterByTrack)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Configurator as Conf
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.HTTP.Conduit as HTTP
import qualified System.Console.CmdArgs.Implicit as CA

data Ibox = Ibox { config :: FilePath }
    deriving (Show, Data, Typeable)

programName :: String
programName = "ibox"

args :: CA.Mode (CA.CmgArgs Ibox)
args = CA.cmdArgsMode $ Ibox { config = CA.def CA.& = CA.args }
    CA.& = CA.summary (unwords [programName, showVersion, version])
    CA.& = CA.program programName

main :: IO ()
main = do
    mainArgs <- CA.cmdArgsRun args
    let confFile = config mainArgs
    if null confFile
        then print $ helpText [] HelpFormatDefault args
        else runBot confFile

runBot :: FilePath -> IO()
runBot confFile = do
    conf <- Conf.load [Conf.Required confFile]
    username <- Conf.lookupDefault "" conf "userName"
    twInfo <- getTWInfoFromEnv conf
    userId <- getUserId conf
    when (isNothing userId) $ error "accessToken must contain a'-'"
    void . HTTP.withManager $ \mngr -> do
        leftIO . T.putStrLn $ T.unwords ["Listening for Tweets to", username, "..."]

    let lenv = LtxbotEnv {
        envUserId = fromJust userId,
        envTwInfo = twInfo,
        envManager = mngr
    }

    src <- stream twInfo mngr (statusesFilterByTrack $ T.contact ["@", username])
    src C.$ =+ normalizeMentions C.$$+- CL.mapM_ (^! act ((~runReaderT`) lenv) . actTL)

where
    getUserId :: Config -> IO (Maybe UserId)
    getUserId conf = do
        maybeUid <- liftIO $ fmap (listToMaybe . T.split (== '-')) (Conf.lookupDefalut "" conf "accessToken")
        return $ fmap (read . T.unpack) maybeUid