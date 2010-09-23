module TestBasePrimitiveRecursion
    where

import Test.HUnit
import Test.QuickCheck

import Natural
import BasePrimitiveRecursion
import Utils

instance Arbitrary Natural where
    arbitrary =
        do
          x <- choose (0,100)
          return (Natural x)

{--
instance Arbitrary Function where
    arbitrary = sized function'
        -- generates a function with arity n
        where function' 0 = Zero
              function' 1 = oneof [Successor,projection 1,composition 1, recursion 1]
              function' k | k > 1 = oneof [projection k,composition k,recursion k]
        where projection k = liftM (Projection i k)

TODO: figure out how to lift this --> I don't think k-arity is the right bet either, maybe
just k-depth
--}                                  

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

checkConstant :: Natural -> Bool
checkConstant n = eval (constant n) [] == n

checkComparison :: Natural -> Natural -> Bool
checkComparison x y = let gteResult = (eval gte [x,y] == 1)
                          ltResult = (eval lt [x,y] == 1)
                      in
                        if (x < y) then 
                            (not gteResult) && ltResult
                        else 
                            gteResult && (not ltResult)

checkAlways :: [Natural] -> Natural -> Bool
checkAlways n x = 
    let numArgs = toNatural $ fromIntegral $ length n
    in
      eval (always numArgs (constant x)) n == x

newtype SmallNatural = Small Integer deriving Show

instance Arbitrary SmallNatural where
    arbitrary =
        do
          x <- choose (0,4)
          return (Small x)

checkPow :: SmallNatural -> SmallNatural -> Bool
checkPow (Small a) (Small b) = eval pow [a,b] == b^a

testAlways1 = TestCase (assertEqual
                        "testAlways1"
                        4
                        (eval (always 3 (constant 4)) [1,2,3]))

testAlways2 = TestCase (assertEqual
                        "testAlways2"
                        3
                        (eval (always 3 (Projection 3 3)) [1,2,3]))

alwaysTests = [ testAlways1, testAlways2 ]

-- find x < 10 such that x >= 5
testMu1 = TestCase (assertEqual 
                   "testMu1"
                   5
                   (eval (mu (comp2 gte (Projection 1 1) (always 1 (constant 5))) (constant 10)) []))

-- find x < 10 such that x * 2 >= 5
testMu2 = TestCase (assertEqual
                    "testMu2"
                    3
                    (eval (mu (comp2 gte (comp2 multiplication (Projection 1 1) (always 1 (constant 2))) (always 1 (constant 5))) (constant 10)) []))

-- find x < 10 such that there is a y <= x with x * y >= 20
-- TODO: code that

muTests = [ testMu1, testMu2 ]

doQuickChecks = do
  performQuickCheck "addition" checkAddition
  performQuickCheck "subtraction" checkSubtraction
  performQuickCheck "multiplication" checkMultiplication
  performQuickCheck "identity" checkIdentity
  performQuickCheck "nonidentity" checkNonIdentity
  performQuickCheck "always" checkAlways
  performQuickCheck "constant" checkConstant
  performQuickCheck "comparison" checkComparison
  performQuickCheck "pow" checkPow

doHUnitTests = do 
  runTestTT (TestList (muTests ++ alwaysTests)) 