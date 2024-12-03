module Utils.Crypto.Hash where

import qualified Crypto.Hash as CH
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

data HashAlgorithm = 
    SHA256 
  | SHA512 
  | Blake2b_512

newtype HashResult = HashResult BS.ByteString 