{-# LANGUAGE DataKinds #-}
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

module Ethereum.Solidity.Meta where

import Language.Haskell.TH
import Network.Web3.Dapp.EthABI.TH
import Network.Web3.Dapp.EthABI.Types
import System.Directory (getCurrentDirectory)

-- Quitar comentario y usar una dirección conocida, sino produce error
-- de compilación
{-
$(downloadHttp "http://192.168.2.101:8545"
      defaultSwarmSettings{ swarmBzzApiHost = "192.168.2.101"}
      $ HexEthAddr "0x3c635e7d4a73a09db2317f0abca7243913908929")
-}

-- Comentar código al quitar los comentarios anteriores.
{--}
$(runIO getCurrentDirectory >>= \wd -> compile (SolcSettings [] [])
    [ wd ++ "/src/Ethereum/Solidity/coin.sol"
    ])
{--}

