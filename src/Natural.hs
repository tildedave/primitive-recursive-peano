{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Natural
    where

newtype Natural = Natural Integer deriving (Eq, Ord, Show, Enum)

toNatural :: Integer -> Natural
toNatural n = if n >= 0 then (Natural n) else error "Cannot convert to natural"

fromNatural :: Natural -> Integer
fromNatural (Natural n) = n

instance Num Natural where
    fromInteger = toNatural
    x + y               = toNatural (fromNatural x + fromNatural y)
    x - y               = let r = fromNatural x - fromNatural y in
                            if r < 0 then error $ "Unnatural subtraction: " ++ (show r)
                                     else toNatural r
    x * y               = toNatural (fromNatural x * fromNatural y)
    abs x               = x
    signum n | n == 0 = 0
             | otherwise = 1

{--
newtype PositiveNatural = Natural Integer deriving (Eq, Ord, Show)

toPositiveNatural :: Integer -> PositiveNatural
toPositiveNatural n = if (n > 0) then (Natural n) else error "not positive natural"

fromPositiveNatural :: PositiveNatural -> Integer
fromPositiveNatural (PositiveNatural n) = n

instance Num PositiveNatural where
    fromInteger = toPositiveNatural
    x + y       = toNatural (fromNatural x + fromNatural
--}
