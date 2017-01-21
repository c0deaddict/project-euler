module Part2.Problem59 where

import           Data.Bits                  (xor)
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Char                  (chr, ord)
import           Data.List                  (find, isInfixOf)
import           Data.List.Split            (splitOn)
import           Network.HTTP.Conduit

--
-- Problem59: XOR decryption
--
-- Each character on a computer is assigned a unique code and the preferred
-- standard is ASCII (American Standard Code for Information Interchange).
-- For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
--
-- A modern encryption method is to take a text file, convert the bytes to
-- ASCII, then XOR each byte with a given value, taken from a secret key. The
-- advantage with the XOR function is that using the same encryption key on the
-- cipher text, restores the plain text; for example, 65 XOR 42 = 107, then
-- 107 XOR 42 = 65.
--
-- For unbreakable encryption, the key is the same length as the plain text
-- message, and the key is made up of random bytes. The user would keep the
-- encrypted message and the encryption key in different locations, and without
-- both "halves", it is impossible to decrypt the message.
--
-- Unfortunately, this method is impractical for most users, so the modified
-- method is to use a password as a key. If the password is shorter than the
-- message, which is likely, the key is repeated cyclically throughout the
-- message. The balance for this method is using a sufficiently long password
-- key for security, but short enough to be memorable.
--
-- Your task has been made easy, as the encryption key consists of three lower
-- case characters. Using cipher.txt (right click and 'Save Link/Target As...'),
-- a file containing the encrypted ASCII codes, and the knowledge that the
-- plain text must contain common English words, decrypt the message and find
-- the sum of the ASCII values in the original text.

problem59 :: IO Int
problem59 = do
  cipher <- readCipher <$> fetchCipher
  let (Just key) = findKey enumerateKeys cipher
  let text = decodeCipher cipher key
  print (asciiToString text)
  return $ sum text

findKey :: [[Int]] -> [Int] -> Maybe [Int]
findKey keys cipher = find (isPlaintext . asciiToString . decodeCipher cipher) keys

enumerateKeys :: [[Int]]
enumerateKeys =
  [ map ord [a,b,c]
  | a <- ['a'..'z']
  , b <- ['a'..'z']
  , c <- ['a'..'z']
  ]

isPlaintext :: String -> Bool
isPlaintext text =
  all isPrintable text &&
  all (`isInfixOf` text) commonEnglishWords

commonEnglishWords :: [String]
commonEnglishWords = ["the", "be", "to", "of", "and"]

isPrintable :: Char -> Bool
isPrintable c = ord c >= 32 && ord c < 127

asciiToString :: [Int] -> String
asciiToString = map chr

decodeCipher :: [Int] -> [Int] -> [Int]
decodeCipher cipher key = map (uncurry xor) cipherAndKey where
  cipherAndKey = zip cipher repeatingKey
  repeatingKey = concat $ repeat key

fetchCipher :: IO String
fetchCipher = do
  bs <- simpleHttp "https://projecteuler.net/project/resources/p059_cipher.txt"
  return $ L.unpack bs

readCipher :: String -> [Int]
readCipher str = fmap read . splitOn "," $ head $ lines str
