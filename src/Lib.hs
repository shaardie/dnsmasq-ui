{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( run
    )
where


import           Control.Monad.IO.Class
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Maybe
import           GHC.Generics
import           Web.Scotty
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static

run :: IO ()
run = scotty 3000 $ do
    -- Logging middleware
    middleware logStdoutDev
    -- Serve Static files
    middleware $ staticPolicy (hasPrefix "static/" >-> addBase ".")
    -- Dnsmasq API
    get "/api/dnsmasq/" getDnsmasq
    get "/" $ redirect "/static/index.html"

data DnsmasqEntry = DnsmasqEntry {
    expirationTime :: Int,
    linkAddress :: String,
    ipv4 :: String,
    hostname :: String,
    clientIdentifier :: String
} deriving (Generic, Show)

instance ToJSON DnsmasqEntry
instance FromJSON DnsmasqEntry

parseDnsmasqLine :: String -> Maybe DnsmasqEntry
parseDnsmasqLine s
    | length ss == 5 = Just DnsmasqEntry
        { expirationTime   = read (head ss) :: Int
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

getDnsmasq :: ActionM ()
getDnsmasq = do
    contents <- liftIO $ readDnsmasq "dnsmasq.leases"
    json contents
