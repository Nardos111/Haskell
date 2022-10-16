{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
module Main (main) where

checkNumber :: Float -> IO ()
checkNumber number = do
  if number == 0
    then putStrLn "Neither positive nor negative"
    else
      if number > 0
        then putStrLn "Positive"
        else putStrLn "Negative"

checkNumbers :: Float -> Float -> IO ()
checkNumbers x y = do
  if x > 0 && y > 0
    then putStrLn "Both are positive"
    else
      if x < 0 && y < 0
        then putStrLn "Both are negative"
        else putStrLn "One is negative other is positive"

isTrue :: IO Bool
isTrue = do
  putStrLn "Executing isTrue()"
  return True

isFalse :: IO Bool
isFalse = do
  putStrLn "Executing isFalse()"
  return False

main :: IO ()
main = do
  checkNumber (-4)
  checkNumber 4
  checkNumber 0
  checkNumbers 9 8
  checkNumbers 9 (-8)

  let letter = "C"

  case letter of
    "A" -> putStrLn "First letter in the alphabet"
    "B" -> putStrLn "Second letter in the alphabet"
    "C" -> putStrLn "Third letter in the alphabet"
    _ -> putStrLn "Don't know what that is"

--   if (isTrue) && (isFalse)
--     then putStrLn "True"
--     else
--       if (isTrue) || (isFalse)
--         then putStrLn "True"
--         else putStrLn "False"