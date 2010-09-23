module Main
    where

import Test.QuickCheck
import Control.Monad
import qualified TestBasePrimitiveRecursion

main = 
    do TestBasePrimitiveRecursion.doQuickChecks
       TestBasePrimitiveRecursion.doHUnitTests