import Control.Monad (forM_)
import Data.Bits (clearBit, setBit, shiftL, testBit, zeroBits)
import Data.ByteString.Lazy as ByteString (ByteString, pack, putStr, readFile, unpack)
import Data.Map (elems, keys)
import Data.Word (Word8)
import Hist (Histogram, buildHistogram, normalizeHistogram)
import Huffman (buildEncodings, buildHuffmanTree, decodeHuffman, encodeHuffman)
import System.Environment (getArgs)
import Text.Printf (printf)

toListBE :: Word8 -> [Bool]
toListBE b = Prelude.map (testBit b) (reverse [0 .. 7])

fromListBE :: [Bool] -> Word8
fromListBE = foldr setter zeroBits . reverse
  where
    setter f c = (if f then setBit else clearBit) (shiftL c 1) 0

partition :: Int -> [a] -> [[a]]
partition n xs = partition' n xs []
  where
    partition' _ [] acc = reverse acc
    partition' n xs acc = partition' n (drop n xs) (take n xs : acc)

bitsToWords :: [Bool] -> [Word8]
bitsToWords = fmap fromListBE . partition 8

inmemoryBitsToByteString :: [Bool] -> ByteString
inmemoryBitsToByteString = ByteString.pack . bitsToWords

filler :: Char
filler = '▓'

consoleWidth :: Int
consoleWidth = 120

getStringBar :: Int -> String
getStringBar h = replicate h filler

doReportHeader :: IO ()
doReportHeader = printf "Byte: Count | Histogram\n"

doSingleLineReport :: Word8 -> Int -> Int -> IO ()
doSingleLineReport byte counter normed =
  printf "%4d:%6d | %s\n" byte counter (getStringBar normed)

doReport :: Histogram Word8 -> IO ()
doReport h = do
  let normalizedHistogram = normalizeHistogram consoleWidth h
  let existingBytes = keys h

  doReportHeader
  forM_
    (zip3 existingBytes (elems h) (elems normalizedHistogram))
    (\(byte, counter, normed) -> doSingleLineReport byte counter normed)

drawHist :: String -> IO ()
drawHist filename = ByteString.readFile filename >>= doReport . buildHistogram . unpack

compress :: String -> IO ()
compress filename = do
  bytes <- unpack <$> ByteString.readFile filename
  let encodings = buildEncodings $ buildHuffmanTree $ buildHistogram bytes
  ByteString.putStr $ inmemoryBitsToByteString $ encodeHuffman encodings bytes

parse :: [String] -> IO ()
parse ["-h"] = usage
parse ["--hist", filename] = drawHist filename
parse ["--compress", filename] = compress filename
parse ["--decompress", filename] = error "Not implemented"
parse _ = usage

usage = print "Usage: ./Main [-h] [{--hist, --compress, --decompress} file]"

main = getArgs >>= parse