{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ethereum.Solidity.Test1 where

import Language.Haskell.TH
import Network.Web3.Dapp.EthABI.TH
import Network.Web3.Dapp.EthABI.Types
import System.Directory (getCurrentDirectory)

$(runIO getCurrentDirectory >>= \wd -> compile (SolcSettings
    [ SolcRemapping
        Nothing
        "github.com/modular-network/ethereum-libraries/"
        (Just $ wd ++ "/library/modular-network/ethereum-libraries/")
    , SolcRemapping
        Nothing
        "github.com/TheLinuxKitten/ethlibs/"
        (Just $ wd ++ "/library/ethlibs/")
    ] ["Owned", "Priced", "StringUtilsLib"])
    [ wd ++ "/src/Ethereum/Solidity/test1.sol"
    ])


