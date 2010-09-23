module Utils
    where

import Test.QuickCheck

performQuickCheck str qc = 
    do
      putStr ("checking " ++ str ++ ": ")
      quickCheck qc
      