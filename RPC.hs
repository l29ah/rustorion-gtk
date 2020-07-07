{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module RPC
	( rpcConnect
	, rpcHandle
	, getView
	, setActions
	) where

import Control.Concurrent.MVar
import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Either
import Data.Maybe
import Data.MessagePack as MP
import Data.PEM (pemParseBS, pemContent)
import qualified Data.Text as T
import Data.X509 as X509
import Data.X509.Memory
import Network.Connection
import Network.TLS
import Network.TLS.Extra.Cipher

import Types

readSignedObject :: FilePath -> IO [Either String (SignedExact Certificate)]
readSignedObject file = do
	content <- B.readFile file
	return $ either error (map (X509.decodeSignedObject . pemContent)) $ pemParseBS content

readCert :: FilePath -> IO (Either String (SignedExact Certificate))
readCert fn = do
	objs <- readSignedObject fn
	pure $ head objs

readKey fn = do
	pems <- readPEMFile fn
	pure $ fromJust $ head $ pemToKey [] $ head pems
	--forM_ pems $ \pem -> do
	--	let content = either (error . show) id $ decodeASN1' BER (pemContent pem)
	--			privkey = catMaybes $ pemToKey [] pem
	--pure privkey

readPEMFile file = do
	content <- B.readFile file
	return $ either error id $ pemParseBS content

provideClientCert :: FilePath -> FilePath -> ([CertificateType], Maybe [HashAndSignatureAlgorithm], [DistinguishedName]) -> IO (Maybe (CertificateChain, PrivKey))
provideClientCert key cert _ = do
	privkey <- readKey key
	cert <- readCert cert
	pure $ Just (CertificateChain [fromRight (error "no certificate in the supplied PEM file") cert], privkey)

recvMessage :: Connection -> IO B.ByteString
recvMessage conn = do
	len <- connectionGetExact conn 4
	connectionGetExact conn $ fromIntegral $ runGet getWord32be $ BL.fromStrict len

sendMessage :: Connection -> B.ByteString -> IO ()
sendMessage conn msg = do
	connectionPut conn $ BL.toStrict $ runPut $ putWord32be $ fromIntegral $ B.length msg
	connectionPut conn msg
	

-- we call the server's methods
rpc :: (MessagePack a) => MVar Connection -> T.Text -> a -> IO B.ByteString
rpc connM method args = withMVar connM $ \conn -> do
	let msg = BL.toStrict $ pack $ ObjectArray [ObjectStr "rustorion-server-0", ObjectStr method, ObjectBin $ BL.toStrict $ pack args]
	sendMessage conn msg
	reply <- recvMessage conn
	[ObjectBool rpcSuccess, ObjectBin retVal] <- unpack $ BL.fromStrict reply
	when (rpcSuccess == False) $ fail $ "rpc failed calling " ++ T.unpack method
	pure retVal

-- server calling our methods over a backconn
rpcHandle :: MVar Connection -> IO ()
rpcHandle connM = withMVar connM $ \conn -> do
	req <- recvMessage conn
	parsedReq <- (unpack $ BL.fromStrict req) :: IO Object
	print parsedReq
	-- send a bogus reply
	sendMessage conn $ BL.toStrict $ pack [ObjectBool True, ObjectBin ""]

getView :: MVar Connection -> IO UniverseView
getView conn = rpc conn "get_view" () >>= unpack . BL.fromStrict

setActions :: MVar Connection -> [Action] -> IO ()
setActions conn acts = rpc conn "set_actions" acts >>= print

rpcConnect host port key cert = do
	ctx <- initConnectionContext
	let tlsClientParams = (defaultParamsClient host mempty)
		{ clientSupported = def { supportedCiphers = ciphersuite_default }
		-- accept any server cert
		, clientHooks = def
			{ onServerCertificate = \_ _ _ _ -> pure []
			, onCertificateRequest = provideClientCert key cert
			}
		}
	let tls = Just $ TLSSettings tlsClientParams
	conn <- connectTo ctx $ ConnectionParams host port tls Nothing
	newMVar conn
