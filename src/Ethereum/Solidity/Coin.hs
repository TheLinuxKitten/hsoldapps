{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Ethereum.Solidity.Coin where

import Language.Haskell.TH
import Network.Web3.Dapp.EthABI.TH
import Network.Web3.Dapp.EthABI.Types
import System.Directory (getCurrentDirectory)

$(runIO getCurrentDirectory >>= \wd -> compile (SolcSettings [] [])
    [ wd ++ "/src/Ethereum/Solidity/coin.sol"
    ])


