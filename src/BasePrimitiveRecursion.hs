module BasePrimitiveRecursion
    where

import Natural
import Debug.Trace

data Function = 
    Zero |
    Successor | 
    Projection Natural Natural | 
    Composition Natural Natural Function [Function] |
    Recursion Natural Function Function  |
    DebugThunk ([Natural] -> String) Function 

instance Show Function where
    show Zero = "Zero"
    show Successor = "Successor"
    show (Projection i j) = "Projection " ++ show i ++ " " ++ show j
    show (Composition m n f gs) = show f ++ "(" ++ show gs ++ ")"
    show (Recursion n f g) = "Recursion " ++ show n ++ " " ++ show f  ++ " " ++ show g

arity exp = 
    case exp of 
      Zero -> 0
      Successor -> 1
      Projection i n -> n
      Composition k m f gs -> m
      Recursion k f g -> k + 1
      DebugThunk df f -> arity f

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
                     wffComposition = (arity f == k) && (head gsArity == m) && 
                                      (allSame gsArity) && (wff f) && (wffList gs)
                 in
                   if (wffComposition) then 
                       True 
                   else
                       error ("Invalid composition: " ++ (show exp) ++ "\n" ++ 
                              "f arity: " ++ show (arity f) ++ " (expected " ++ (show k) ++ ")\n" ++
                              "gs arity: " ++ (show gsArity) ++ " (expected " ++ (show m)))
      Recursion k f g -> let wffRecursion = (wff f) && (wff g) && (arity f == k) && 
                                            (arity g == k + 2)
                         in
                           if (wffRecursion) then
                               True
                           else
                               error ("Invalid recursion: " ++ show (exp))
      DebugThunk df f -> wff f
    where wffList es = (foldr (&&) True (map wff es))


-- TODO: thread Maybe monad through here
eval :: Function -> [Natural] -> Natural
eval exp args = 
    let evalResult = case exp of
                       Zero -> case args of 
                                 [] -> 0
                       Successor -> case args of
                                      [x] -> (x + 1)
                       Projection i n -> let index = fromInteger (fromNatural (i - 1)) 
                                         in
                                           if (index >= length args) then
                                               error ("Invalid projection: " ++ (show exp) ++ " -- only " ++ (show (length args)) ++ " args")
                                           else
                                               args !! index
                       Composition k m f gs -> let egs = map (\g -> eval g args) gs
                                               in
                                                 eval f egs
                       Recursion k f g -> case args of
                                            (0:xs) -> eval f xs
                                            (n:xs) -> let prec = n - 1 
                                                          prCall = eval exp (prec:xs)
                                                      in
                                                        eval g (prec:prCall:xs)
                       DebugThunk df f -> let result = (eval f args) 
                                          in
                                            trace (df args) result
    in
      evalResult

comp f gs = 
    let gArity = if (length gs == 0) then
                     0 
                 else
                     arity (head gs)
    in
      Composition (arity f) gArity f gs

comp1 f g1 = comp f [g1]
comp2 f g1 g2 = comp f [g1,g2]
comp3 f g1 g2 g3 = comp f [g1,g2,g3]
comp4 f g1 g2 g3 g4 = comp f [g1,g2,g3,g4]


partialComp n f gs = 
    let wrappedGs = map (always n) gs
    in
      Composition (arity f) n f (wrappedGs ++ [(Projection i n) | i <- [1..n]])

primRecursion f g = Recursion (arity f) f g

constant 0 = Zero
constant n = let nminus1Expr = constant (n - 1)
             in
                 comp1 Successor (nminus1Expr)

--always k f = Composition (arity f) k f [(Projection i k) | i <- [1..k]]
always k f = Composition (arity f) k f []

addition = let baseFunction = (Projection 1 1)  
               stepFunction = comp1 Successor (Projection 2 3)
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
                  stepFunction = comp1 predecessor (Projection 2 3)
              in
                primRecursion baseFunction stepFunction

multiplication = let baseFunction = (always 1 Zero)
                     stepFunction = (Composition 2 3 addition 
                                                     [Projection 2 3,Projection 3 3])
                 in
                   primRecursion baseFunction stepFunction

-- pow(a,b) = b^a
-- pow(a+1,b) = b^a * b
-- pow(a+1,b) = g(a,pow(a,b),b)
pow = let baseFunction = (always 1 (constant 1))
          stepFunction = comp2 multiplication (Projection 2 3) (Projection 3 3)
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

lt = comp1 nonZero subtraction
gte = comp1 isZero subtraction

identity = let baseFunction = Zero
               stepFunction = comp1 Successor (Projection 1 2)
           in
             primRecursion baseFunction stepFunction

prIf n f g = let baseFunction = f
                 stepFunction = (Composition n (n+2) g
                                 [(Projection k (n+2)) | k <- [3..n+2]])
           in
             primRecursion f stepFunction

condZero h f g = let n = arity h 
                   in 
                     Composition (n+1) n (prIf n f g) (h:[(Projection k n) | k <- [1..n]])

-- x == y if x \< y and y \< x
{--
equals = condZero
         (Composition 2 2 lt [(Projection 1 2),(Projection 2 2)])
         (condZero
          (Composition 2 2 lt [(Projection 2 2),(Projection 1 2)])
          (always 2 (constant 1))
          (always 2 (constant 0)))
         (always 2 (constant 0))
--}

equals = 
    let x1 = Projection 1 2
        x2 = Projection 2 2
        trueAnswer = (always 2 (constant 1))
        falseAnswer = (always 2 (constant 0))
    in
      condZero (comp2 lt x1 x2)
                     (condZero
                      (comp2 lt x2 x1)
                      trueAnswer
                      falseAnswer)
                     falseAnswer

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
prMax = condZero lt (Projection 1 2) (Projection 2 2)
prMaximumList n = prFoldArgsLeft n prMax (always n Zero)

-- starting i is the first argument
prMin = condZero lt (Projection 2 2) (Projection 1 2)
prMinimumList n = prFoldArgsLeft n prMin (Projection 1 n)

fold n f i = 
    let baseFunction = i
        stepFunction = Composition 2 (n+2) f [comp1 Successor (Projection 1 (n+2)),(Projection 2 (n+2))]
    in
      primRecursion baseFunction stepFunction

factorial = fold 0 multiplication (always 0 (constant 1))
sumAll = fold 0 addition (always 0 (constant 0))

-- n args
-- f is the function to check, takes n args
-- bounded by k

{--
eval_fAtJ n f j args = 
    let k = arity f 
    in 
      Composition k n f (j:args)

baseMu n f p = let k = arity f
                   eval_fAtZero = eval_fAtJ n f (always n (constant 0)) [(Projection i n) | i <- [1..n]]
             in
               condZero eval_fAtZero p (always n (constant 0))

stepMu n f p = 
    let i = comp1 Successor (Projection 1 (n + 2))
        r = Projection 2 (n + 2)
        eval_fAtCurrent = eval_fAtJ (n + 2) f i [(Projection i (n + 2)) | i <- [3..n+2]]  -- arity n
    in
      (Composition 2 (n + 2)
                   prMin 
                   [(condZero eval_fAtCurrent
                                    p
                                    i),
                    r])

mu f p =
    let k = arity f
        n = k - 1
        baseFunction = baseMu n f p
        stepFunction = stepMu n f p 
    in
      Composition (n + 1) n
                      (primRecursion baseFunction stepFunction) 
                      (p:[(Projection k n) | k <- [1..n]])
--}

-- n = args to f
baseMu2 n f = 
    let m = (Projection 1 (n + 1))
    in
      (condZero (comp f ((always (n + 1) Zero):[Projection i (n + 1) | i <- [2..n+1]]))
                m
                (always (n + 1) Zero))

stepMu2 n f =
    -- arity should be n + 2
    -- stepMu2(n+1,pr(n,----),MAX,----) = 
    let i = Projection 1 (n + 3)
        r = Projection 2 (n + 3)
        m = Projection 3 (n + 3)
    in
      (condZero (comp2 lt r m) 
                    (condZero (comp f (i:[Projection j (n + 3) | j <- [4..n+3]]))
                              m
                              i)
                    r)

mu f p = 
    let k = (arity f) - 1
        baseFunction = baseMu2 (k+1) f
        stepFunction = stepMu2 (k+1) f
    in
      (comp
       (primRecursion baseFunction stepFunction)
       (p:p:[(Projection j k) | j <- [1..k]]))
                     
exists f p =
    let k = arity f
        n = k - 1
        muSearch = mu f p
    in
      condZero (comp2 gte muSearch p) (always n (constant 1)) (always n (constant 0))

-- modCheck c d a b = b * d + c == a
modCheck = (comp2 equals (comp2 addition (comp2 multiplication (Projection 1 4) (Projection 4 4)) (Projection 2 4)) (Projection 3 4))
-- mod x y = mod x y
mod = 
    let b = Projection 2 2
    in
      condZero (Projection 1 2) (always 2 (constant 0)) (condZero (comp2 equals (Projection 2 2) (always 2 (constant 1)))
                                                                      (mu (exists modCheck (Projection 2 3)) b)
                                                                      (always 2 (constant 0)))

-- isprime n = THERE DOES NOT EXIST A K < N WITH MOD K N = 0
-- isprime n = if EXIST A MOD A N == 0 == n THEN 1 ELSE 0
-- primehelper n 
primehelper = 
    let zero = always 2 (constant 0)
        one = always 2 (constant 1)
        a = (Projection 1 2)
        b = (Projection 2 2)
    in
      condZero (comp2 equals zero a) 
               (condZero (comp2 equals one a) 
                         (condZero (comp2 equals a b)
                                       (condZero (comp2 BasePrimitiveRecursion.mod b a) 
                                                 one
                                                 zero)
                                       zero)
                         zero)
               zero
isprime =
    let a = Projection 1 1
        one = always 1 (constant 1)
        zero = always 1 (constant 0)
    in
      condZero (comp2 equals one a) 
               (condZero (exists primehelper a) one zero)
               (zero)
