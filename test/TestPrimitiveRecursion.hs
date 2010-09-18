module TestPrimitiveRecursion
    where
import Test.HUnit
import Test.QuickCheck

import Natural
import PrimitiveRecursion

instance Arbitrary Natural where
    arbitrary =
        do
          x <- choose (1,100)
          return (Natural x)

checkAddition :: Natural -> Natural -> Bool
checkAddition x y = eval addition [x,y] == x + y

-- verify subtract(a,b) = max(b - a,0)
checkSubtraction :: Natural -> Natural -> Bool
checkSubtraction x y = eval subtraction [x,y] == (if (y < x) then 0 else y - x)

checkMultiplication :: Natural -> Natural -> Bool
checkMultiplication x y = eval multiplication [x,y] == x * y

checkIdentity :: Natural -> Bool
checkIdentity x = eval equals [x,x] == 1

checkNonIdentity :: Natural -> Natural -> Bool
checkNonIdentity x y = eval equals [x,y] == if (x == y) then 1 else 0

doQuickChecks = do
  quickCheck checkAddition
  quickCheck checkSubtraction
  quickCheck checkMultiplication
  quickCheck checkIdentity
  quickCheck checkNonIdentity