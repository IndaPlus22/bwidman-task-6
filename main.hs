module F1 where

import Data.Char

main :: IO ()
main = putStrLn (show (fib 40))

-- Fibonacci-talen
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = 2 * fib (n - 2) + fib (n - 3)

-- Rövarspråket
konsonant :: Char -> Bool
konsonant c = not (elem c ['a', 'e', 'i', 'o', 'u', 'y'])

rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak (x:xs) =
    if konsonant x
    then [x, 'o', x] ++ rovarsprak xs
    else x:rovarsprak xs

karpsravor :: String -> String
karpsravor [] = []
karpsravor s@(x:xs) =
    if konsonant x
    then x:karpsravor (drop 3 s)
    else x:karpsravor xs

-- Medellängd
antalBokstaver :: String -> Double
antalBokstaver [] = 0.0
antalBokstaver (x:xs) =
    if isAlpha x
    then 1.0 + antalBokstaver xs
    else antalBokstaver xs

antalOrd :: String -> Double
antalOrd [] = 0.0
antalOrd (x:xs) =
    if isAlpha x && (null xs || not (isAlpha (head xs)))
    then 1.0 + antalOrd xs
    else antalOrd xs

medellangd :: String -> Double
medellangd [] = 0.0
medellangd s = antalBokstaver s / antalOrd s

-- Listskyffling
varannan :: [x] -> [x]
varannan [] = []
varannan [x] = [x]
varannan (x:xs) = x:varannan (tail xs)

skyffla :: [x] -> [x]
skyffla [] = []
skyffla list@(x:xs) = varannan list ++ skyffla (varannan xs)