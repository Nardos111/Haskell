module Lib
  ( someFunc,
  )
where

z :: Float
z = 3.14

x :: Integer
x = 1

y :: Bool
y = True

someFunc :: IO ()
someFunc = do
  print (z, x, y)
  print (z, y)

--   print (x + z)
