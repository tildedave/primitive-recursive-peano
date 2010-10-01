{--
  Goal for this file:
   * Higher level representation of Primitive Recursive functions to make it easier to program in.
   * Support "cheat" evaluations for computationally complex operations (prime factoring is important for list tuples;
     base modulus implementation is very inefficient for numbers > 50, so some solution is needed to get around this difficulty while 
     maintaining the spirit of the project)
   * Automatically handle arity, arguments. 
   * Higher order primitive recursive functions to automatically handle projecting arguments (not 100% sure how this is going to work).
   * Tests: Verify translation from higher level PR functions to lower level PR functions using QuickCheck.

   * Code in this file should be able to handle lists and contain the basic machinery for traversing a list.
   * Verifying proofs would be annoying to write in Haskell so writing it in PR subset of Haskell is sure to also be annoying --
     figure out how to do this nicely!
--}


module PrimitiveRecursion
    where

import Natural

data Function = 
              Zero |
              