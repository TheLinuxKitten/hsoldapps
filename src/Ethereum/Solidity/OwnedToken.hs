{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ethereum.Solidity.OwnedToken where

import Language.Haskell.TH
import Network.Web3.Dapp.EthABI.TH
import Network.Web3.Dapp.EthABI.Types
import System.Directory (getCurrentDirectory)

$(runIO getCurrentDirectory >>= \wd -> compile (SolcSettings [] ["TokenCreator"])
    [ wd ++ "/src/Ethereum/Solidity/ownedtoken.sol"
    ])


