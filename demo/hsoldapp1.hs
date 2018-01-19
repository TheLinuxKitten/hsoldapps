{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Ethereum.Solidity.Coin
import Ethereum.Solidity.New1
import Ethereum.Solidity.OwnedToken
import Ethereum.Solidity.Sharer
import Ethereum.Solidity.Test1
import Ethereum.Solidity.Topics
import Ethereum.Solidity.Types

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import Data.Either (isLeft, isRight)
import Data.List (nubBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Network.Web3
import Network.Web3.Dapp.EthABI
import Network.Web3.Dapp.EthABI.Types
import Network.Web3.Extra
import Network.Web3.Types
import System.Environment (getArgs, getEnv)

getOps :: FilePath -> IO (Bool,Bool,Bool,Bool,String,[(Text,HexEthAddr)])
getOps h = pAs (False,False,False,False,h++"/.ethereum/geth.ipc",[]) <$> getArgs
  where
    pAs (th,dok,log,useHttp,uri,ha) ("--ipc":ipc:as) = pAs (th,dok,log,False,ipc,ha) as
    pAs (th,dok,log,useHttp,uri,ha) ("--http":url:as) = pAs (th,dok,log,True,url,ha) as
    pAs (th,dok,log,useHttp,uri,ha) ("--log":as) = pAs (th,dok,True,useHttp,uri,ha) as
    pAs (th,dok,log,useHttp,uri,ha) ("--doOk":as) = pAs (th,True,log,useHttp,uri,ha) as
    pAs (th,dok,log,useHttp,uri,ha) ("--threaded":as) = pAs (True,dok,log,useHttp,uri,ha) as
    pAs (th,dok,log,useHttp,uri,ha) ("--contractAddress":caddr:as) = pAs (th,dok,log,useHttp,uri,cAddr caddr : ha) as
    pAs _ ("--help":as) = msgUso
    pAs _ ("-h":as) = msgUso
    pAs ops [] = ops
    pAs _ _ = msgUso
    cAddr caddr = let cs = splitOn ":" caddr in (T.pack $ head cs, HexEthAddr $ T.pack $ last cs)
    msgUso = error $ "Uso:\n"
                  ++ "    [--ipc file|--http url]\n"
                  ++ "    [--log]\n"
                  ++ "    [--doOk]\n"
                  ++ "    [--threaded]\n"
                  ++ "    [--contractAddress <contractN>:<0xaddress>]    (sino crea contrato. multiple)\n"

enviaTx :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
        => HexEthAddr -> Maybe HexEthAddr -> HexData
        -> Web3T c m (Either Text RpcEthTxReceipt)
enviaTx addrFrom mAddrTo dat = web3_estimateAndSendTx addrFrom mAddrTo Nothing (Just dat)

getCAddr n caddrs = snd <$> listToMaybe (filter ((n==) . fst) caddrs)

main :: IO ()
main = do
  h <- getEnv "HOME"
  (threaded,doOk,doLog,useHttp,uri,caddrs) <- getOps h
  let ll = if doLog then LevelDebug else LevelInfo
  resp <- runStdoutLoggingT $ filterLoggerLogLevel ll $ if useHttp
              then runWeb3HttpT 10 15 uri (ethAction threaded doOk caddrs)
              else runWeb3IpcT 10 15 uri (ethAction threaded doOk caddrs)
  print resp
  where
    ethAction threaded doOk caddrs = do
      let tests = [ ("Coin", test_coin)
                  , ("Topics", test_topics)
                  , ("Test1", test_test1)
                  , ("Types", test_types)
                  , ("Types", test_types_simple)
                  , ("New1", test_new1)
                  , ("Sharer", test_sharer)
                  , ("OwnedToken", test_ownedtoken)
                  ]
      if threaded then do
        wasys <- mapM (web3Async . runTest doOk caddrs) tests
        eaddrs <- mapM web3Wait wasys
        showWeb3Session "Session: terminando"
        mapM_ (\((nom,_),addr) -> logConAddr nom addr)
              (nubBy (\d1 d2 -> (snd d1) == (snd d2))
                  $ map (\(td,eaddr) -> (td,fromRight eaddr))
                  $ filter (isRight . snd) $ zip tests eaddrs)
        showWeb3Session "Session: errores"
        mapM_ (logDebugN . T.pack . show) (filter isLeft eaddrs)
      else do
        addrs <- mapM (runTest doOk caddrs) tests
        showWeb3Session "Session: terminando"
        mapM_ (\((nom,_),addr) -> logConAddr nom addr)
              (nubBy (\d1 d2 -> (snd d1) == (snd d2)) $ zip tests addrs)
      return ()
    runTest doOk caddrs (nom,f) = do
      showWeb3Session $ "Session: lanzo " <> nom
      f doOk $ getCAddr nom caddrs
    logConAddr n addr =
      myLog $ "--contractAddress \"" <> n <> ":" <> getHexAddr addr <> "\""

myLog :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
      => Text -> Web3T c m ()
myLog = logInfoN
-- myLog = liftIO . putStrLn . T.unpack

showBalance addr = eth_getBalance addr RPBLatest
               >>= myLog . ((getHexAddr addr <> " balance: ")<>)
                         . T.pack . show

logDebugTxrs :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m, Show e)
             => (RpcEthLog -> Either Text e)
             -> [Either Text RpcEthTxReceipt]
             -> Web3T c m ()
logDebugTxrs dl etxrs = mapM_ (\etxr -> do
  logDebugN $ T.pack $ show etxr
  case etxr of
    Left e -> return ()
    Right txr -> mapM_ (myLog . T.pack . show . dl) (txrLogs txr) ) etxrs

logDebugFilterLogs :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m, Show e)
                   => Text
                   -> (RpcEthLog -> Either Text e)
                   -> [RpcFilterLog]
                   -> Web3T c m ()
logDebugFilterLogs n dl fls =
  mapM_ (myLog . T.pack . (("FilterLogs " ++ show n ++ ": ")++)
               . (\rfl -> case rfl of
                    EthHashFilterLog h -> show h
                    EthFilterLog fl -> show $ dl fl)) fls

conAddr :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m, Show e)
        => (HexEthAddr -> Web3T c m ())
        -> Maybe (RpcEthLog -> Either Text e)
        -> (HexEthAddr, Maybe HexEthAddr, Maybe Integer, Maybe HexData)
        -> Maybe HexEthAddr
        -> Web3T c m HexEthAddr
conAddr conG mdl (f,mt,mv,md) maddr = case maddr of
  Just addr -> conG addr >> return addr
  Nothing -> do
    txr <- web3_estimateAndSendTx f mt mv md >>= web3FromE
    case mdl of
      Nothing -> return ()
      Just dl -> logDebugTxrs dl [Right txr]
    return $ fromJust $ txrContractAddress txr

newFilter :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
          => HexEthAddr -> Int64 -> [RpcEthFilterTopic] -> Web3T c m FilterId
newFilter cAddr bn =
  web3FilterNew . RpcEthFilter (Just $ RPBNum bn) (Just RPBLatest) (Just [cAddr])
                . Just

test_new1 doOk maddr = do
  (addr1:addr2:addr3:addr4:accs) <- eth_accounts
  cAddr <- conAddr c_guard null_decode_log (c_new_sendtx addr2) maddr
  etxrs1 <- web3_estimateAndSendTxs
            [ c_created_sendtx addr2 cAddr 50000000
            , c_created_sendtx addr2 cAddr 50000000
            , c_createandendowd_sendtx addr2 cAddr 10000000 (50000000,400)
            ]
  logDebugN $ T.pack $ show etxrs1
  return cAddr

test_coin doOk maddr = do
  (addr1:addr2:addr3:addr4:accs) <- eth_accounts
  cAddr <- conAddr coin_guard (Just coin_decode_log) (coin_new_sendtx addr2) maddr
  bn <- eth_blockNumber
  fi1 <- newFilter cAddr bn $ coin_to_filter_topics Coin_Sent_Filter
  etxrs1 <- web3_estimateAndSendTxs
            [ coin_mint_sendtx addr2 cAddr (addr1, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr2, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr3, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr4, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr2, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr3, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr4, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr2, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr3, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr4, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr2, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr3, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr4, 50000000)
            , coin_mint_sendtx addr1 cAddr (addr4, 50000000)
            , coin_mint_sendtx addr1 cAddr (addr2, 50000000)
            , coin_mint_sendtx addr1 cAddr (addr3, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr2, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr3, 50000000)
            , coin_mint_sendtx addr2 cAddr (addr4, 50000000)
            , coin_mint_sendtx addr1 cAddr (addr4, 50000000)
            , coin_mint_sendtx addr1 cAddr (addr2, 50000000)
            , coin_mint_sendtx addr1 cAddr (addr3, 50000000)
            , coin_mint_sendtx addr1 cAddr (addr4, 50000000)
            ]
  etxrs2 <- web3_estimateAndSendTxs
            [ coin_send_sendtx addr1 cAddr (addr3, 500)
            , coin_send_sendtx addr2 cAddr (addr3, 500)
            , coin_send_sendtx addr4 cAddr (addr3, 500)
            , coin_send_sendtx addr4 cAddr (addr2, 500)
            , coin_send_sendtx addr1 cAddr (addr2, 500)
            , coin_send_sendtx addr1 cAddr (addr2, 333)
            , coin_send_sendtx addr1 cAddr (addr3, 333)
            , coin_send_sendtx addr4 cAddr (addr3, 333)
            ]
  logDebugTxrs coin_decode_log etxrs1
  logDebugTxrs coin_decode_log etxrs2
  mapM_ (\addr -> web3_call addr cAddr (coin_balances_in addr)
                      >>= myLog . ((getHexAddr addr <> ": ") <>)
                                . T.pack . show . coin_balances_out)
        [addr1, addr2, addr3, addr4]
  web3FilterGetChanges fi1 >>= logDebugFilterLogs "coin filter1" coin_decode_log
  showWeb3Session "Session: finalizando Coin"
  when doOk $ web3FilterUninstall fi1
  return cAddr

test_topics doOk maddr = do
  (addr1:addr2:addr3:addr4:accs) <- eth_accounts
  cAddr <- conAddr topics_guard (Just topics_decode_log) (topics_new_sendtx addr1) maddr
  bn <- eth_blockNumber
  fi1 <- newFilter cAddr bn $ topics_to_filter_topics
          $ Topics_EvString_Filter (Just "string")
  fi2 <- newFilter cAddr bn $ topics_to_filter_topics
          $ Topics_EvBytes_Filter (Just "bytestring")
  fi3 <- newFilter cAddr bn $ topics_to_filter_topics
          $ Topics_EvArrUint_Filter (Just [4])
  fi4 <- newFilter cAddr bn $ topics_to_filter_topics
          $ Topics_EvArrUint_Filter (Just [0,1,2,3])
  fi5 <- newFilter cAddr bn $ topics_to_filter_topics
          $ Topics_EvArrUint_Filter (Just [1])
  fi6 <- newFilter cAddr bn $ topics_to_filter_topics
          $ Topics_EvArrUint48_Filter (Just [0xffffffffff])
  etxrs1 <- web3_estimateAndSendTxs
            [ topics_sendevbytes_sendtx addr1 cAddr "bytestring"    -- "bytestring"
            , topics_sendevstring_sendtx addr1 cAddr "string"       -- "string"
            , topics_sendevstring_sendtx addr1 cAddr "áéíóú"        -- "áéíóú"
            , topics_sendevarrbool_sendtx addr1 cAddr [True]        -- uint(1)
            , topics_sendevarrbool_sendtx addr1 cAddr [False,True,False,True]  -- (uint(0),uint(1),uint(0),uint(1))
            , topics_sendevarruint48_sendtx addr1 cAddr [0xffffffffff]  -- uint(0xffffffffff)
            , topics_sendevarruint48_sendtx addr1 cAddr [0xaaaaaaaaaa]
            , topics_sendevarruint_sendtx addr1 cAddr [1]            -- uint(1)
            , topics_sendevarruint_sendtx addr1 cAddr [1]
            , topics_sendevarruint_sendtx addr1 cAddr [4]
            , topics_sendevarruint_sendtx addr1 cAddr [4]
            , topics_sendevarruint_sendtx addr1 cAddr [6]
            , topics_sendevarruint_sendtx addr1 cAddr [5]
            , topics_sendevarruint_sendtx addr1 cAddr [4]
            , topics_sendevarruint_sendtx addr1 cAddr [0,1,2,3]
            , topics_sendevarruint_sendtx addr1 cAddr [0,1,2,3,4]
            , topics_sendevarrbytes16_sendtx addr1 cAddr ["bytes16"]  -- bytes32("bytes16")
            , topics_sendevarrbytes16_sendtx addr1 cAddr ["bytes16","bytes16","bytes16","bytes16"]  -- (bytes32("bytes16"),bytes32("bytes16"),bytes32("bytes16"),bytes32("bytes16"))
            , topics_sendevarrbytes32_sendtx addr1 cAddr ["bytes32"]  -- bytes32("bytes32")
            , topics_sendevarrbytes32_sendtx addr1 cAddr ["bytes32","bytes32","bytes32","bytes32"]  -- (bytes32("bytes32"),bytes32("bytes32"),bytes32("bytes32"),bytes32("bytes32"))
            , topics_sendevstring_sendtx addr1 cAddr "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz"    -- "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz"
            , topics_sendevarruint_sendtx addr1 cAddr [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40] -- (uint(0),uint(1),uint(2),uint(3), .., uint(40))
            , topics_sendevarrbytes32_sendtx addr1 cAddr ["abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz","abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz","abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz","abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz"] -- ("456789abcdefghijklmnopqrstuvwxyz","456789abcdefghijklmnopqrstuvwxyz","456789abcdefghijklmnopqrstuvwxyz","456789abcdefghijklmnopqrstuvwxyz")
            ]
  logDebugTxrs topics_decode_log etxrs1
  web3FilterGetChanges fi1 >>= logDebugFilterLogs "topics filter1" topics_decode_log
  web3FilterGetChanges fi2 >>= logDebugFilterLogs "topics filter2" topics_decode_log
  web3FilterGetChanges fi3 >>= logDebugFilterLogs "topics filter3" topics_decode_log
  web3FilterGetChanges fi4 >>= logDebugFilterLogs "topics filter4" topics_decode_log
  web3FilterGetChanges fi5 >>= logDebugFilterLogs "topics filter5" topics_decode_log
  web3FilterGetChanges fi6 >>= logDebugFilterLogs "topics filter6" topics_decode_log
  showWeb3Session "Session: finalizando Topics"
  when doOk $ web3FilterUninstall fi1
  when doOk $ web3FilterUninstall fi2
  when doOk $ web3FilterUninstall fi3
  when doOk $ web3FilterUninstall fi4
  when doOk $ web3FilterUninstall fi5
  when doOk $ web3FilterUninstall fi6
  return cAddr

test_test1 doOk maddr = do
  (addr1:addr2:addr3:addr4:accs) <- eth_accounts
  cAddr <- conAddr test1_guard (Just test1_decode_log) (test1_new_sendtx addr3 "test1") maddr
  bn <- eth_blockNumber
  fi0 <- newFilter cAddr bn $ test1_to_filter_topics $ Test1_SendTokens_Filter (Nothing, Just addr1)
  fi1 <- web3FilterNew $ RpcEthFilter (Just $ RPBNum bn) (Just RPBLatest)
                                      (Just [cAddr]) Nothing
  fi2 <- newFilter cAddr bn [topicNull, topicAddr addr1]
  fi3 <- newFilter cAddr bn [topicNull, topicAddr addr1, topicNull]
  fi4 <- newFilter cAddr bn $ test1_to_filter_topics $ Test1_Nombre_Filter (Just "Contract actualizado: test1 iniciado.")
  wf1 <- web3Fork $ do
    _ <- web3_estimateAndSendTxs
          [ test1_buytokens_sendtx addr1 cAddr 50000
          , test1_buytokens_sendtx addr2 cAddr 50000
          , test1_buytokens_sendtx addr1 cAddr 50000
          , test1_buytokens_sendtx addr2 cAddr 50000
          ]
    return ()
  wf2 <- web3Fork $ do
    _ <- web3_estimateAndSendTxs
          [ test1_buytokens_sendtx addr3 cAddr 50000
          , test1_buytokens_sendtx addr3 cAddr 50000
          , test1_buytokens_sendtx addr4 cAddr 50000
          , test1_buytokens_sendtx addr4 cAddr 50000
          ]
    return ()
  wf3 <- web3Fork $ do
    _ <- web3_estimateAndSendTxs
          [ test1_buytokens_sendtx addr1 cAddr 50000
          , test1_buytokens_sendtx addr2 cAddr 50000
          , test1_buytokens_sendtx addr1 cAddr 50000
          , test1_buytokens_sendtx addr2 cAddr 50000
          ]
    return ()
  wf4 <- web3Fork $ do
    _ <- web3_estimateAndSendTxs
          [ test1_buytokens_sendtx addr3 cAddr 50000
          , test1_buytokens_sendtx addr3 cAddr 50000
          , test1_buytokens_sendtx addr4 cAddr 50000
          , test1_buytokens_sendtx addr4 cAddr 50000
          ]
    return ()
  watxrs1 <- web3Async $ do
    etxrs1 <- web3_estimateAndSendTxs
              [ test1_buytokens_sendtx addr1 cAddr 50000
              , test1_buytokens_sendtx addr2 cAddr 50000
              , test1_buytokens_sendtx addr1 cAddr 50000
              , test1_buytokens_sendtx addr2 cAddr 50000
              ]
    etxrs2 <- web3_estimateAndSendTxs
              [ test1_buytokens_sendtx addr1 cAddr 50000
              , test1_buytokens_sendtx addr1 cAddr 50000
              , test1_buytokens_sendtx addr2 cAddr 50000
              , test1_buytokens_sendtx addr1 cAddr 50000
              , test1_sendtokens_sendtx addr2 cAddr (addr3,22)
              , test1_sendtokens_sendtx addr1 cAddr (addr3,11)
              , test1_sendtokens_sendtx addr1 cAddr (addr4,11)
              , test1_sendtokens_sendtx addr2 cAddr (addr4,11)
              ]
    return $ etxrs1 ++ etxrs2
  watxrs2 <- web3Async $ do
    etxrs1 <- web3_estimateAndSendTxs
              [ test1_buytokens_sendtx addr3 cAddr 50000
              , test1_buytokens_sendtx addr3 cAddr 50000
              , test1_buytokens_sendtx addr4 cAddr 50000
              , test1_buytokens_sendtx addr4 cAddr 50000
              ]
    etxrs2 <- web3_estimateAndSendTxs
              [ test1_buytokens_sendtx addr4 cAddr 50000
              , test1_buytokens_sendtx addr3 cAddr 50000
              , test1_buytokens_sendtx addr4 cAddr 50000
              , test1_buytokens_sendtx addr3 cAddr 50000
              , test1_sendtokens_sendtx addr4 cAddr (addr2,22)
              , test1_sendtokens_sendtx addr3 cAddr (addr2,11)
              , test1_sendtokens_sendtx addr4 cAddr (addr2,11)
              , test1_sendtokens_sendtx addr3 cAddr (addr2,11)
              ]
    return $ etxrs1 ++ etxrs2
  etxrs1 <- web3_estimateAndSendTxs
            [ test1_buytokens_sendtx addr1 cAddr 50000
            , test1_buytokens_sendtx addr2 cAddr 50000
            , test1_buytokens_sendtx addr3 cAddr 50000
            , test1_buytokens_sendtx addr4 cAddr 50000
            ]
  etxrs2 <- web3_estimateAndSendTxs
            [ test1_cambiatokenprice_sendtx addr3 cAddr 10
            , test1_cambianombre_sendtx addr3 cAddr "test1 iniciado."
            ]
  etxrs3 <- web3_estimateAndSendTxs
            [ test1_buytokens_sendtx addr1 cAddr 50000
            , test1_buytokens_sendtx addr4 cAddr 50000
            , test1_buytokens_sendtx addr2 cAddr 50000
            , test1_buytokens_sendtx addr3 cAddr 50000
            , test1_sendtokens_sendtx addr2 cAddr (addr1,22)
            , test1_sendtokens_sendtx addr3 cAddr (addr1,11)
            , test1_sendtokens_sendtx addr1 cAddr (addr4,11)
            , test1_sendtokens_sendtx addr2 cAddr (addr4,11)
            ]
  showWeb3Session "Session: procesando Test1"
  logDebugN "============================================================="
  etxrs4 <- fromRight <$> web3Wait watxrs1
  etxrs5 <- fromRight <$> web3Wait watxrs2
  logDebugN "========= Síncronas ========================================="
  logDebugTxrs test1_decode_log etxrs1
  logDebugTxrs test1_decode_log etxrs2
  logDebugTxrs test1_decode_log etxrs3
  logDebugN "========= Asíncronas ========================================"
  logDebugTxrs test1_decode_log etxrs4
  logDebugTxrs test1_decode_log etxrs5
  logDebugN "============================================================="
  showBalance cAddr
  test1_nombre_call addr3 cAddr >>= myLog . T.pack . ("nombre: "++) . show
  test1_tokenprice_call addr3 cAddr >>= myLog . T.pack . ("token price: "++) . show
  eth_accounts >>= (\accs' ->
    mapM_ (\acc -> test1_tokens_call addr3 cAddr acc
               >>= myLog . T.pack . ((show acc ++ " tokens: ")++) . show) accs')
  logDebugN "================ filter changes 1 ==========================="
  web3FilterGetChanges fi0 >>= logDebugFilterLogs "test1 filter0" test1_decode_log
  web3FilterGetChanges fi1 >>= logDebugFilterLogs "test1 filter1" test1_decode_log
  web3FilterGetChanges fi2 >>= logDebugFilterLogs "test1 filter2" test1_decode_log
  web3FilterGetChanges fi3 >>= logDebugFilterLogs "test1 filter3" test1_decode_log
  web3FilterGetChanges fi4 >>= logDebugFilterLogs "test1 filter4" test1_decode_log
  logDebugN "================ filter changes 2 ==========================="
  web3FilterGetChanges fi0 >>= logDebugFilterLogs "test1 filter0" test1_decode_log
  web3FilterGetChanges fi1 >>= logDebugFilterLogs "test1 filter1" test1_decode_log
  web3FilterGetChanges fi2 >>= logDebugFilterLogs "test1 filter2" test1_decode_log
  web3FilterGetChanges fi3 >>= logDebugFilterLogs "test1 filter3" test1_decode_log
  web3FilterGetChanges fi4 >>= logDebugFilterLogs "test1 filter4" test1_decode_log
  showWeb3Session "Session: finalizando Test1"
  when doOk $ web3FilterUninstall fi0
  web3FilterUninstall fi1
  when doOk $ web3FilterUninstall fi2
  when doOk $ web3FilterUninstall fi3
  when doOk $ web3FilterUninstall fi4
  web3ForkWait wf1
  when doOk $ web3ForkWait wf2
  when doOk $ web3ForkWait wf3
  when doOk $ web3ForkWait wf4
  return cAddr

test_types_simple doOk maddr = do
  (addr1:addr2:addr3:addr4:accs) <- eth_accounts
  cAddr <- conAddr types_guard (Just types_decode_log) (types_new_sendtx addr2) maddr
  eth_call (RpcEthMsgCall Nothing (Just cAddr) Nothing Nothing Nothing
            (Just $ types_func3_in (12345,True,23))) RPBLatest
    >>= myLog . T.pack . show . types_func3_out
  return cAddr

test_types doOk maddr = do
  (addr1:addr2:addr3:addr4:accs) <- eth_accounts
  cAddr <- conAddr types_guard (Just types_decode_log) (types_new_sendtx addr2) maddr
  bn <- eth_blockNumber
  fi <- web3FilterNew $ RpcEthFilter (Just $ RPBNum bn) (Just RPBLatest)
                                     (Just [cAddr]) Nothing
  types_retuint_call_pure cAddr 1234567890 >>= myLog . T.pack . show
  types_retaddress_call_pure cAddr addr4 >>= myLog . T.pack . show
  types_retbytes16_call_pure cAddr "0 1 2 3 4 5 6 7 " >>= myLog . T.pack . show
  types_retbytes32_call_pure cAddr "bytes32" >>= myLog . T.pack . show
  types_retfixarruint_call_pure cAddr [1234567890,987654321]
    >>= myLog . T.pack . show
  types_retfixarraddress_call_pure cAddr [addr3,addr4] >>= myLog . T.pack . show
  types_retfixarrbytes16_call_pure cAddr ["fixarrbytes16","fixarrbytes16"]
    >>= myLog . T.pack . show
  types_retfixarrbytes32_call_pure cAddr ["fixarrbytes32","fixarrbytes32"]
    >>= myLog . T.pack . show
  types_retarruint_call_pure cAddr [9,8,7,6,5,4,3,2,1,0] >>= myLog . T.pack . show
  types_retarraddress_call_pure cAddr [addr1,addr2,addr3,addr4]
    >>= myLog . T.pack . show
  types_retarrbytes16_call_pure cAddr ["bytes16","bytes16"]
    >>= myLog . T.pack . show
  types_retarrbytes32_call_pure cAddr ["bytes32","bytes32"]
    >>= myLog . T.pack . show
  types_retbytes_call_pure cAddr "bytes" >>= myLog . T.pack . show
  types_retstring_call_pure cAddr "string" >>= myLog . T.pack . show
  mapM_ (\addr -> types_valores_call addr1 cAddr addr >>= myLog . T.pack . show) [addr1, addr2, addr3, addr4]
  types_func3_call addr1 cAddr (123456,True,127) >>= myLog . T.pack . show
  types_func4_call_pure cAddr (0xabcdefabcdef, [True,False,True], -123456789, "Play Ethereum", addr3, "bytestring hola", False, ["uno","dos","tres","cuatro"]) >>= myLog . T.pack . show
  types_func5_call_pure cAddr (127,255) >>= myLog . T.pack . show
  types_func5_call_pure cAddr (-127,255) >>= myLog . T.pack . show
  types_func5_call_pure cAddr (-1,255) >>= myLog . T.pack . show
  types_func6_call_pure cAddr >>= myLog . T.pack . show
  types_func7_call_pure cAddr >>= myLog . T.pack . show
  types_func8_call_pure cAddr >>= myLog . T.pack . show
  types_func10_call_pure cAddr >>= myLog . T.pack . show
  types_func11_call_pure cAddr >>= myLog . T.pack . show
  etxrs1 <- web3_estimateAndSendTxs
            [ types_func1_sendtx addr1 cAddr (addr3, 9576)
            , types_func2_sendtx addr1 cAddr (addr2, 9876)
            ]
  etxrs2 <- web3_estimateAndSendTxs
            [ types_retevents_sendtx addr1 cAddr
            , types_reteventfixarruint_sendtx addr1 cAddr [12,34,56,78]
            , types_reteventfixarrbytes16_sendtx addr1 cAddr ["bytes16", "bytes16", "bytes16", "bytes16"]
            , types_reteventfixarrbytes32_sendtx addr1 cAddr ["bytes32", "bytes32", "bytes32", "bytes32"]
            , types_reteventfixarraddress_sendtx addr1 cAddr [addr1, addr2, addr3, addr4]
            , types_reteventarruint_sendtx addr1 cAddr [9912,9934,9956,9978]
            , types_reteventarrbytes16_sendtx addr1 cAddr ["dynbytes16", "dynbytes16", "dynbytes16", "dynbytes16"]
            , types_reteventarrbytes32_sendtx addr1 cAddr ["dynbytes32", "dynbytes32", "dynbytes32", "dynbytes32"]
            , types_reteventarraddress_sendtx addr1 cAddr [addr1, addr2, addr3, addr4, addr3, addr2, addr1]
            , types_retevents2_sendtx addr1 cAddr
            , types_reteventfixarruint2_sendtx addr1 cAddr ([12,34,56,78],[87,65,43,21])
            , types_reteventfixarrbytes162_sendtx addr1 cAddr (["bytes16", "bytes16", "bytes16", "bytes16"],["bytes16", "bytes16", "bytes16", "bytes16"])
            , types_reteventfixarrbytes322_sendtx addr1 cAddr (["bytes32", "bytes32", "bytes32", "bytes32"],["bytes32", "bytes32", "bytes32", "bytes32"])
            , types_reteventfixarraddress2_sendtx addr1 cAddr ([addr1, addr2, addr3, addr4],[addr1, addr2, addr3, addr4])
            , types_reteventarruint2_sendtx addr1 cAddr ([9912,9934,9956,9978],[9912,9934,9956,9978])
            , types_reteventarrbytes162_sendtx addr1 cAddr (["dynbytes16", "dynbytes16", "dynbytes16", "dynbytes16"],["dynbytes16", "dynbytes16", "dynbytes16", "dynbytes16"])
            , types_reteventarrbytes322_sendtx addr1 cAddr (["dynbytes32", "dynbytes32", "dynbytes32", "dynbytes32"],["dynbytes32", "dynbytes32", "dynbytes32", "dynbytes32"])
            , types_reteventarraddress2_sendtx addr1 cAddr ([addr1, addr2, addr3, addr4, addr3, addr2, addr1],[addr1, addr2, addr3, addr4, addr3, addr2, addr1])
            , types_retevents5_sendtx addr1 cAddr
            ]
  logDebugTxrs types_decode_log etxrs1
  logDebugTxrs types_decode_log etxrs2
  web3FilterGetChanges fi >>= logDebugFilterLogs "types" types_decode_log
  liftIO (threadDelay $ 45*10^6)
  showWeb3Session "Session: finalizando Types"
  when doOk $ web3FilterUninstall fi
  return cAddr

test_sharer doOk maddr = do
  (addr1:addr2:addr3:addr4:accs) <- eth_accounts
  cAddr <- conAddr sharer_guard (Just sharer_decode_log)
                      (sharer_new_sendtx addr1) maddr
  logInfoN "Sharer: antes de transferir"
  showBalance addr1
  showBalance addr2
  showBalance addr3
  showBalance addr4
  etxrs1 <- web3_estimateAndSendTxs
            [ sharer_sendhalf_sendtx addr1 cAddr 2000 addr4
            , sharer_sendhalf_sendtx addr1 cAddr 3006 addr2
            , sharer_sendhalf_sendtx addr1 cAddr 34566 addr2
            , sharer_sendhalf_sendtx addr1 cAddr 31563 addr2
            , sharer_sendhalf_sendtx addr1 cAddr 8975 addr3
            , sharer_sendhalf_sendtx addr2 cAddr 8975 addr3
            , sharer_sendhalf_sendtx addr3 cAddr 8975 addr2
            , sharer_sendhalf_sendtx addr4 cAddr 8975 addr2
            , sharer_sendhalf_sendtx addr1 cAddr 98012 addr4
            ]
  logDebugTxrs sharer_decode_log etxrs1
  logInfoN "Sharer: despues de transferir"
  showBalance addr1
  showBalance addr2
  showBalance addr3
  showBalance addr4
  showWeb3Session "Session: finalizando Sharer"
  return cAddr

test_ownedtoken doOk maddr = do
  (addr1:addr2:addr3:addr4:accs) <- eth_accounts
  cAddr <- conAddr ownedtoken_guard null_decode_log
                      (ownedtoken_new_sendtx addr4 "ownedtoken") maddr
  etxr1 <- web3_estimateAndSendTx'
            (ownedtoken_changename_sendtx addr4 cAddr "ownedtoken2")
  etxr2 <- web3_estimateAndSendTx'
            (ownedtoken_transfer_sendtx addr4 cAddr addr2)
  etxr3 <- web3_estimateAndSendTx'
            (ownedtoken_changename_sendtx addr2 cAddr "ownedtoken3")
  etxrs4 <- web3_estimateAndSendTxs
              [ ownedtoken_transfer_sendtx addr2 cAddr addr3
              , ownedtoken_transfer_sendtx addr4 cAddr addr2
              ]
  mapM_ (logDebugN . T.pack . show) (etxr1:etxr2:etxr3:etxrs4)
  showWeb3Session "Session: finalizando OwnedToken"
  return cAddr

