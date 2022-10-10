import Data.Char

module F1 where

main :: IO ()
main = putStrLn (show (fib 42))

-- Fibonacci-talen
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Rövarspråket
konsonant :: Char -> Bool
konsonant c = not (elem c ['a', 'e', 'i', 'o', 'u', 'y'])

rovarsprak :: String -> String
rovarsprak [] = []
rovarsprak (head:tail) =
    if konsonant head
    then [head, 'o', head] ++ rovarsprak tail
    else [head] ++ rovarsprak tail

karpsravor :: String -> String
karpsravor [] = []
karpsravor s@(head:tail) =
    if konsonant head
    then [head] ++ karpsravor (drop 3 s)
    else [head] ++ karpsravor tail

-- Medellängd
medellangd :: String -> Double
medellangd [] = 0.0
medellangd s = 1.0

-- Listskyffling

skyffla s = s