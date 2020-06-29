{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
import System.Environment

import Types

readSignedObject file = do
	content <- B.readFile file
	return $ either error (map (X509.decodeSignedObject . pemContent)) $ pemParseBS content

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
	pure $ Just (CertificateChain [fromRight undefined cert], privkey)

rpc conn method = do
	let msg = BL.toStrict $ pack $ ObjectArray [ObjectStr "rustorion-server-0", ObjectStr method, ObjectArray []]
	connectionPut conn $ BL.toStrict $ runPut $ putWord32be $ fromIntegral $ B.length msg
	connectionPut conn msg
	len <- connectionGetExact conn 4
	reply <- connectionGetExact conn $ fromIntegral $ runGet getWord32be $ BL.fromStrict len
	[ObjectBool rpcSuccess, ObjectBin retVal] <- unpack $ BL.fromStrict reply
	when (rpcSuccess == False) $ fail $ "rpc failed calling " ++ T.unpack method
	pure retVal

getView :: Connection -> IO UniverseView
getView conn = rpc conn "get_view" >>= unpack . BL.fromStrict

main = do
	[key, cert] <- getArgs
	ctx <- initConnectionContext
	let host = "localhost"
	let port = 4433
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
	view <- getView conn
	print view
