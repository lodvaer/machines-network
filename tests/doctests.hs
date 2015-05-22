import Test.DocTest

import Data.CaseInsensitive (mk)
import Data.List (isSuffixOf)

main :: IO ()
main = modules >>= doctest . ("-isrc":)

modules :: IO [String]
modules = cut . words <$> readFile "machines-network.cabal"
  where
    cut = takeWhile (not . isSuffixOf ":") . drop 1 .
            dropWhile ((/=mk "exposed-modules:").mk)
