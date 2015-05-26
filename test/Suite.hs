module Main where

import  Test.Framework (defaultMain)

import qualified Data.DateTimeTest

main :: IO ()
main = defaultMain $ Data.DateTimeTest.tests
