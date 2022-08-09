module Main where
import           Text.Read
import System.Environment (getArgs)

data Tower = A | B | C deriving (Show,Eq)
data Action = Act Tower Tower
  deriving Eq
instance (Show Action) where
  show (Act a b) = show a ++ "->" ++ show b
move :: Tower -> Tower -> Action
move = Act
hanoi :: Int -> Tower -> Tower -> Tower -> [Action]
hanoi 1 a b c = [move a c]
hanoi n a b c = hanoi (n - 1) a c b ++ [move a c] ++ hanoi (n - 1) b a c
main :: IO ()
main = do
  numS <- getArgs
  let num = readMaybe $ head numS :: Maybe Int
  case num of
    Nothing -> putStrLn "It's not a number!"
    Just x  -> do 
      putStrLn "The solution is:"
      putItem $ hanoi x A B C
 where
  putItem (x : xs)
    | xs == [] = print x
    | xs /= [] = do
      print x
      putItem xs
