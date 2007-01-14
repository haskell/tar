module Properties where

import Codec.Archive.Tar

import qualified Data.ByteString.Lazy.Char8 as BS
import Test.QuickCheck

instance Arbitrary Char where
    arbitrary = elements $ ['/'] ++ ['a'..'e']

-- splitFileName

test_splitFileName f n p = n > 0 && not (null p) ==> let (x,y) = splitFileName n p in f x y

prop_splitFileName_id n p = test_splitFileName (\x y -> x++y == p) n p

prop_splitFileName_len n p = test_splitFileName (\x y -> length y <= n) n p

prop_splitFileName_nonempty = test_splitFileName $ \x y -> not (null y)

prop_splitFileName_noroot = test_splitFileName $ \x y -> null x || head y /= '/'


-- setPart

prop_setPart_len n new old = BS.length (setPart n new old) == BS.length old