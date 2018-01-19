{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ethereum.Solidity.Meta where

import Language.Haskell.TH
import Network.Web3.Dapp.EthABI.TH
import Network.Web3.Dapp.EthABI.Types

$(downloadHttp "http://192.168.122.201:8543" defaultSwarmSettings
      $ HexEthAddr "0x39df13a96f46db1e62b673086c692736d74baa1f")

