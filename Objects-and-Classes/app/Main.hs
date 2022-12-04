module Main (main) where

import Lib

main :: IO ()
main = do

class Person a where
    firstName :: a -> String
    age :: a -> Int
    getFullName :: a -> String
data Employee = Employee
    {   employeeFirstName :: String
    ,   employeeLastName :: String
    ,   employeeAge :: Int
    ,   company :: String
    ,   email :: String
    ,   salary :: Int

    }

instance Person Employee where
    firstName = employeeFirstName
    age = employeeAge
    getFullName e = employeeFirstName e ++ " " ++ employeeLastname e 
