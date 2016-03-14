import Data.Attoparsec.ByteString as A
import Data.ByteString as B
import Data.Word

h_SHA1     = 0x11 :: Word8
h_SHA2_256 = 0x12 :: Word8
h_SHA2_512 = 0x13 :: Word8
h_SHA3     = 0x14 :: Word8
h_BLAKE2B  = 0x40 :: Word8
h_BLAKE2S  = 0x41 :: Word8

data DecodedMultihash = DecodedMultihash {
	code :: Int, 
	name :: String, -- not included in multihash, duh
	h_length :: Int, 
	digest :: B.ByteString
} deriving (Show)

type Multihash = B.ByteString

encode :: B.ByteString -> String -> Multihash 
encode b h = B.append hash_info b where 
	hash_info = B.pack [(get_hash_code h),(fromIntegral $ B.length b)]

decode :: Multihash -> Maybe DecodedMultihash 
decode b = case parseOnly parse_multihash b of 
	Left _-> Nothing 
	Right a -> Just dmh where
		dmh = a {h_length = digest_length} where
		digest_length = B.length $ digest a 


parse_multihash :: Parser DecodedMultihash
parse_multihash = do b_code <- A.anyWord8 
                     A.take 1 
                     b_digest <- A.takeByteString
                     return (DecodedMultihash {code=(fromIntegral b_code), name=(get_hash_string b_code), h_length=0, digest=b_digest})


get_hash_string :: Word8 -> String 
get_hash_string c = case c of 
	h_SHA1 -> "sha1"
	h_SHA2_256 ->"sha2-256"
	h_SHA2_512 ->"sha2-512"
	h_SHA3  -> "sha3"
	h_BLAKE2B -> "blake2b"
	h_BLAKE2S -> "blake2s"

get_hash_code :: String -> Word8
get_hash_code s = case s of 
	"sha1" -> h_SHA1
	"sha2-256" -> h_SHA2_256
	"sha2-512" -> h_SHA2_512
	"sha3" -> h_SHA3 
	"blake2b" -> h_BLAKE2B
	"blake2s" -> h_BLAKE2S
	
