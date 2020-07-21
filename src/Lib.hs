{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Text.Read                      ( readMaybe )
import           Data.Maybe
import           GHC.Generics
import           Web.Scotty
import           Network.Wai.Handler.Warp       ( setHost
                                                , setPort
                                                , defaultSettings
                                                , HostPreference
                                                )
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Data.Configurator
import           Data.Streaming.Network.Internal
                                                ( HostPreference(Host) )

run :: IO ()
run = do
    -- Configuration
    cfg <- load
        [ Optional "/etc/dnsmasq-ui.conf"
        , Optional "$(HOME)/.dnsmasq-ui.conf"
        , Optional "dnsmasq-ui.conf"
        ]
    host <- lookupDefault "127.0.0.1" cfg "server.host" :: IO String
    port              <- lookupDefault 3000 cfg "server.port" :: IO Int
    staticDir         <- lookupDefault "." cfg "server.static" :: IO FilePath
    dnsmasqLeasesFile <-
        lookupDefault "/var/lib/misc/dnsmasq.leases" cfg "dnsmasq.leases" :: IO
            FilePath

    let options =
            Options 1 $ setPort port $ setHost (Host host) defaultSettings

    scottyOpts options $ do
        -- Logging middleware
        middleware logStdoutDev
        -- Serve Static files
        middleware $ staticPolicy (hasPrefix "static/" >-> addBase staticDir)
        -- Dnsmasq API
        get "/api/v1/dnsmasq/" $ getDnsmasq dnsmasqLeasesFile
        get "/" $ redirect "/static/index.html?refresh=10"

data DnsmasqEntry = DnsmasqEntry {
    expirationTime :: Int,
    linkAddress :: String,
    ipv4 :: String,
    hostname :: String,
    clientIdentifier :: String
} deriving (Generic, Show, Eq)

instance ToJSON DnsmasqEntry
instance FromJSON DnsmasqEntry

parseDnsmasqLine :: String -> Maybe DnsmasqEntry
parseDnsmasqLine s
    | length ss == 5 = case readMaybe (head ss) :: Maybe Int of
        Nothing -> Nothing
        Just e  -> Just DnsmasqEntry { expirationTime   = e
                                     , linkAddress      = ss !! 1
                                     , ipv4             = ss !! 2
                                     , hostname         = ss !! 3
                                     , clientIdentifier = ss !! 4
                                     }
    | otherwise = Nothing
    where ss = words s

parseDnsmasq :: String -> [DnsmasqEntry]
parseDnsmasq s = mapMaybe parseDnsmasqLine $ lines s


readDnsmasq :: FilePath -> IO [DnsmasqEntry]
readDnsmasq fp = do
    contents <- readFile fp
    return $ parseDnsmasq contents

getDnsmasq :: FilePath -> ActionM ()
getDnsmasq fp = do
    contents <- liftIO $ readDnsmasq fp
    json contents
