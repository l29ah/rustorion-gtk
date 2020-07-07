module CertGen where

import Crypto.Hash.Algorithms
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15
import Data.ASN1.BinaryEncoding
import Data.ASN1.Encoding
import Data.ASN1.Types
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.Either
import Data.PEM
import Data.X509
import Time.Types

genKeypair :: IO (PublicKey, PrivateKey)
genKeypair = do
	let exponent = 0x10001
	let keyLength = 4096 `div` 8
	generate keyLength exponent

makeKeyCert keyfile certfile = do
	(publicKey, privateKey) <- genKeypair
	B.writeFile keyfile $ pemWriteBS $ PEM "RSA PRIVATE KEY" [] $ BL.toStrict $ encodeASN1 DER $ ((toASN1 $ PrivKeyRSA privateKey) [])

	let genericPubKey = PubKeyRSA publicKey
	let cert = Certificate {
		certVersion = 2 -- There are three different versions of X.509.
		-- The certificate has to claim what version it uses.
		-- So you'll find this Version: field in the certificate.
		-- The most recent version is 3 and this is the most used version.
		-- X.509 version 3 defines extensions.
		-- The version is encoded with base 0, so version 3 is denoted as 3-1 =
		-- 2
		, certSerial = 1
		, certSignatureAlg = SignatureALG HashSHA256 (pubkeyToAlg genericPubKey)
		, certIssuerDN = DistinguishedName []
		, certValidity =
			( DateTime {dtDate = Date 2000 January 1, dtTime = TimeOfDay 0 0 0 0}
			, DateTime {dtDate = Date 3000 December 31, dtTime = TimeOfDay 23 59 0 0}
			)
		, certSubjectDN = DistinguishedName []
		, certPubKey = genericPubKey
		, certExtensions = Extensions $ Just [extensionEncode True (ExtKeyUsage [KeyUsage_digitalSignature, KeyUsage_keyEncipherment])]
		}
	let sign dat = do
		signedData <- fmap (fromRight undefined) $ signSafer (Just SHA256) privateKey dat
		pure (signedData, certSignatureAlg cert)
	signed_exact_obj <- objectToSignedExactF sign cert
	B.writeFile certfile $ pemWriteBS $ PEM "CERTIFICATE" [] $ encodeSignedObject signed_exact_obj
