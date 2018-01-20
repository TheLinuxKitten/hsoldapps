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

-- Quitar comentario y usar una dirección conocida, sino produce error
-- de compilación
{-
$(downloadHttp "http://192.168.122.201:8543" defaultSwarmSettings
      $ HexEthAddr "0x39df13a96f46db1e62b673086c692736d74baa1f")
-}

-- Comentar código al quitar los comentarios anteriores.
$(runIO getCurrentDirectory >>= \wd -> compile (SolcSettings [] [])
    [ wd ++ "/src/Ethereum/Solidity/coin.sol"
    ])

