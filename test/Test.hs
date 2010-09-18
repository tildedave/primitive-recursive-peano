module Main
    where

import Test.QuickCheck
import Control.Monad
import qualified TestPrimitiveRecursion

main = 
    do TestPrimitiveRecursion.doQuickChecks
