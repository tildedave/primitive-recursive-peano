module Main
    where

import Test.QuickCheck
import Control.Monad
import qualified TestPrimitiveRecursive

main = 
    do forM TestPrimitiveRecursive.quickChecks quickCheck 
