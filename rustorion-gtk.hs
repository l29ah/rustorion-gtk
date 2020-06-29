{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Either
import Data.Maybe
import Data.MessagePack as MP
import Data.PEM (pemParseBS, pemContent)
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
	--forM_ objs $ \o ->
	--	case o of
	--		Left err	 -> error ("decoding Certificate failed: " ++ show err)
	--		Right signed -> do
	--			showCert signed
  where
	--	hashCert signedCert = do
	--		putStrLn ("subject(MD5) old: " ++ hexdump' (X509.hashDN_old subject))
	--		putStrLn ("issuer(MD5) old:  " ++ hexdump' (X509.hashDN_old issuer))
	--		putStrLn ("subject(SHA1):" ++ hexdump' (X509.hashDN subject))
	--		putStrLn ("issuer(SHA1): " ++ hexdump' (X509.hashDN issuer))
	--		where
	--			subject = X509.certSubjectDN cert
	--			issuer  = X509.certIssuerDN cert
	--			cert	= X509.signedObject $ X509.getSigned signedCert

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
	-- FIXME check success
	decRetVal :: UniverseView <- unpack $ BL.fromStrict retVal
	print $ decRetVal

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
	rpc conn "get_view"
