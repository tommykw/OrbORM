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

//