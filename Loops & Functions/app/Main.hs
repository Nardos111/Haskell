module Main (main) where
import Control.Arrow (ArrowLoop(loop))

makeListFromTo :: Int -> Int -> IO ()
makeListFromTo x y = if x <= y
                     then do
                        print x
                        makeListFromTo (x + 1) y
                     else putStrLn "Completed the loop"


-- ==> For loop
-- for (i=0; i<10; i+=2) {
--    do something
-- }

forLoop :: Int -> Int -> Int -> IO()
forLoop loopNum maxLoopNum value =
   if loopNum < maxLoopNum
      then do
         print loopNum
         forLoop (loopNum+value) maxLoopNum value
   else putStrLn "Completed the loop"


-- ==> For each loop
-- foreach (char c in string) 
-- {
--    num = num+1
-- }

forEach :: String -> Int
forEach [] = 0
forEach string = 1 + forEach string


-- ==> While Loop
-- While (a%10 !=0) {
--    a = a+1
-- }

whileLoop :: Int -> IO ()
whileLoop a =
   if a `mod` 10 /=0
   then do
      print a
      whileLoop (a+1)
   else 
      putStrLn "Completed the loop"

doubleNum :: Int -> Int
doubleNum n = 2*n


main = do
    makeListFromTo 1 10
    forLoop 1 10 2
    whileLoop 1
    print(doubleNum 2)
