module PrimitiveRecursion
    where

import Natural
import Debug.Trace

data Function = 
    Zero |
    Successor | 
    Projection Natural Natural | 
    Composition Natural Natural Function [Function] |
    Recursion Natural Function Function 
    deriving Show

arity exp = 
    case exp of 
      Zero -> 0
      Successor -> 1
      Projection i n -> n
      Composition k m f gs -> m
      Recursion k f g -> k + 1

allSame [] = True
allSame [x] = True
allSame (x:y) = (x == head y) && (allSame y)

wff exp = 
    case exp of
      Zero -> True
      Successor -> True
      Projection i n -> (i >= 0) && (i <= n)
      Composition k m f gs -> 
          (case gs of 
            [] -> (arity f == k)
            a -> let gsArity = (map arity gs) 
                             in
                               (arity f == k) && (head gsArity == m) && 
                               (allSame gsArity) && (wff f) && (wffList gs))
      Recursion k f g -> (wff f) && (wff g) && (arity f == k) && 
                        (arity g == k + 2)
    where wffList es = (foldr (&&) True (map wff es))


-- TODO: thread Maybe monad through here
eval exp args = 
    let evalResult = case exp of 
                       Zero -> case args of 
                                 [] -> 0
                       Successor -> case args of
                                      [x] -> (x + 1)
                       Projection i n -> args !! fromInteger (fromNatural (i - 1))
                       Composition k m f gs -> let egs = map (\g -> eval g args) gs
                                               in
                                                 eval f egs
                       Recursion k f g -> case args of
                                            (0:xs) -> eval f xs
                                            (n:xs) -> let prec = n - 1 
                                                          prCall = eval exp (prec:xs)
                                                      in
                                                        eval g (prec:prCall:xs)
    in
      evalResult

comp f g = Composition (arity f) (arity g) f [g]
partialComp n f gs = 
    let wrappedGs = map (always n) gs
    in
      Composition (arity f) n f (wrappedGs ++ [(Projection i n) | i <- [1..n]])

primRecursion f g = Recursion (arity f) f g

constant 0 = Zero
constant n = let nminus1Expr = constant (n - 1)
             in
                 comp Successor nminus1Expr

always k f = Composition (arity f) k f []

addition = let baseFunction = (Projection 1 1)  
               stepFunction = comp Successor (Projection 2 3)
           in
             primRecursion baseFunction stepFunction

predecessor = let baseFunction = Zero  -- pred(0) = 0
                  stepFunction = (Projection 1 2)
              in
                primRecursion baseFunction stepFunction

-- subraction a b = b - a
-- sub(n+1,m) = pred(sub(n,m))
-- sub(n+1,m) = pred(proj_2(n,sub(n,m),m))
subtraction = let baseFunction = (Projection 1 1)
                  stepFunction = comp predecessor (Projection 2 3)
              in
                primRecursion baseFunction stepFunction

multiplication = let baseFunction = (always 1 Zero)
                     stepFunction = (Composition 2 3 addition 
                                                     [Projection 2 3,Projection 3 3])
                 in
                   primRecursion baseFunction stepFunction


isZero = let baseFunction = (always 0 (constant 1))
             stepFunction = (always 2 (constant 0))
         in
           primRecursion baseFunction stepFunction

nonZero = let baseFunction = (always 0 (constant 0))
              stepFunction = (always 2 (constant 1))
          in
            primRecursion baseFunction stepFunction

lt = comp nonZero subtraction
gte = comp isZero subtraction

identity = let baseFunction = Zero
               stepFunction = comp Successor (Projection 1 2)
           in
             primRecursion baseFunction stepFunction

prIf n f g = let baseFunction = f
                 stepFunction = (Composition n (n+2) g
                                 [(Projection k (n+2)) | k <- [3..n+2]])
           in
             primRecursion f stepFunction

prCondZero h f g = let n = arity h 
                   in 
                     Composition (n+1) n (prIf n f g) (h:[(Projection k n) | k <- [1..n]])

-- prFoldLeft(f,i,x1...xn) = f(i,f(x1,f(x2,...,f(xn-1,xn))))
prFoldArgsLeft n f i = 
    let prFoldLeftHelper k n = 
            if (k == n) then 
                (Projection n n)
            else
                (Composition 2 n f [(Projection k n),prFoldLeftHelper (k+1) n])
    in
      (Composition 2 n f [i,prFoldLeftHelper 1 n])

-- prFoldRight(f,i,x1...xn) = f(xn,f(xn-1,...,f(x2,f(i,x1))))
prFoldArgsRight n f i =
    let prFoldRightHelper k n = 
            if (k == 1) then
                (Projection 1 n)
            else
                (Composition 2 n f [(Projection k n),prFoldRightHelper (k-1) n])
    in
      (Composition 2 n f [i,prFoldRightHelper n n])      

-- IDEA: prMax(n1,prMax(n2,prMax(n3,...)))
prMax = prCondZero lt (Projection 1 2) (Projection 2 2)
prMaximumList n = prFoldArgsLeft n prMax (always n Zero)

-- starting i is the first argument
prMin = prCondZero lt (Projection 2 2) (Projection 1 2)
prMinimumList n = prFoldArgsLeft n prMin (Projection 1 n)

foldNat n f i = 
    let baseFunction = i
        stepFunction = Composition 2 (n+2) f [comp Successor (Projection 1 (n+2)),(Projection 2 (n+2))]
    in
      primRecursion baseFunction stepFunction

prFact = foldNat 0 multiplication (always 0 (constant 1))
prTotal = foldNat 0 addition (always 0 (constant 0))

-- n args
-- f is the function to check, takes n args
-- bounded by k

eval_fAtJ n f j = 
    let k = arity f 
    in 
      Composition k n f (j:([(Projection i n) | i <- [1..n]]))

baseMu n f p = let k = arity f
                   eval_fAtZero = eval_fAtJ n f (constant 0)
             in
               prCondZero eval_fAtZero (always n (constant p)) (always n (constant 0))

stepMu n f p = 
    let i = comp Successor (Projection 1 (n + 2))
        r = Projection 2 (n + 2)
        eval_fAtCurrent = eval_fAtJ (n + 2) f i  -- arity n
    in
      (Composition 2 (n + 2)
                   prMin 
                   [(prCondZero eval_fAtCurrent
                                    (always (n + 2) (constant p))
                                    i),
                    r])

mu f p =
    let k = arity f
        n = k - 1
        baseFunction = baseMu n f p
        stepFunction = stepMu n f p 
    in
      Composition n (n + 1) 
                      (primRecursion baseFunction stepFunction) 
                      (always n (constant p):[(Projection k n) | k <- [1..n]])

