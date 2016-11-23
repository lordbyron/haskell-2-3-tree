module Main where

import Tree23
import System.IO

prompt :: Tree Int -> IO ()
prompt t = do
  print t
  putStr "Enter a number: "
  hFlush stdout
  n <- getLine
  prompt (insert t (read n :: Int))

main :: IO ()
main = prompt (Empty :: (Tree Int))
