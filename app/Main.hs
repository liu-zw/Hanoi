module Main where

data Tower = A | B | C deriving (Show,Eq)
data Action = Act Tower Tower deriving (Eq)
instance (Show Action) where
    show (Act a b) = show a ++ "->" ++ show b
move :: Tower -> Tower -> Action
move = Act
hanoi :: Int -> Tower -> Tower -> Tower -> [Action]
hanoi 1 a b c = [move a c]
hanoi n a b c = hanoi (n - 1) a c b ++ [move a c] ++ hanoi (n - 1) b a c
main :: IO ()
main = putItem $ hanoi 3 A B C
  where
    putItem (x : xs)
        | xs == [] = print x
        | xs /= [] = do
            print x
            putItem xs