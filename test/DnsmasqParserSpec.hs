module DnsmasqParserSpec where

import           Test.Hspec
import           Lib

spec :: Spec
spec = do
    describe "parse line in dnsmasq" $ do
        it "returns Nothing if line is not parseable" $ do
            parseDnsmasqLine "" `shouldBe` (Nothing :: Maybe DnsmasqEntry)
            parseDnsmasqLine
                    "1594837a606 de:ad:be:af:de:ad 13.37.13.37 beafdead de:ad:be:af:de:ad"
                `shouldBe` (Nothing :: Maybe DnsmasqEntry)
        it "returns DnsmasqEntry if line can be parsed"
            $          parseDnsmasqLine
                           "1594837606 de:ad:be:af:de:ad 13.37.13.37 beafdead de:ad:be:af:de:ad"
            `shouldBe` Just
                           (DnsmasqEntry 1594837606
                                         "de:ad:be:af:de:ad"
                                         "13.37.13.37"
                                         "beafdead"
                                         "de:ad:be:af:de:ad"
                           )
    describe "parse whole dnsmasq.leases" $ do
        it "returns list of DnsmasqEntries" $ do
            parseDnsmasq
                    (unlines
                        [ "1594837606 de:ad:be:af:de:ad 13.37.13.37 beafdead de:ad:be:af:de:ad"
                        , "broken"
                        ]
                    )
                `shouldBe` [ DnsmasqEntry 1594837606
                                          "de:ad:be:af:de:ad"
                                          "13.37.13.37"
                                          "beafdead"
                                          "de:ad:be:af:de:ad"
                           ]
