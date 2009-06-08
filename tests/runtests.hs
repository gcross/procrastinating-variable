module Main where

import Control.Exception

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.PVar

assertValueThrowsError :: a -> Assertion
assertValueThrowsError expr =
    try ( evaluate expr ) >>= handler
  where
    handler :: Either SomeException a -> Assertion
    handler (Left _) = return ()
    handler (Right _) = assertFailure $ "Error was not thrown!"

assertThrowsError :: IO b -> Assertion
assertThrowsError expr =
    try expr >>= handler
  where
    handler :: Either SomeException a -> Assertion
    handler (Left _) = return ()
    handler (Right _) = assertFailure $ "Error was not thrown!"

test_create_write_read = do
  (pvar,value) <- newPVar
  writePVar pvar True
  assertBool "Does the PVar have the correct value?" value

test_create_read = do
  (pvar,value) <- newPVar
  assertValueThrowsError value

test_create_tryread = do
  (pvar,_) <- newPVar
  maybe_value <- (tryReadPVar pvar :: IO (Maybe Bool))
  assertEqual "Did tryReadPVar obtain the correct value?" Nothing maybe_value

test_create_write_tryread = do
  (pvar,_) <- newPVar
  writePVar pvar True
  maybe_value <- (tryReadPVar pvar :: IO (Maybe Bool))
  assertEqual "Did tryReadPVar obtain the correct value?" (Just True) maybe_value

test_create_write_write_read = do
  (pvar,value) <- newPVar
  writePVar pvar True
  assertThrowsError (writePVar pvar False)
  assertBool "Does the PVar have the correct value?" value

test_create_write_trywrite_read = do
  (pvar,value) <- newPVar
  writePVar pvar True
  result <- tryWritePVar pvar False
  assertEqual "Did tryReadPVar return the correct result?" False result
  assertBool "Does the PVar have the correct value?" value

test_create_trywrite_Read = do
  (pvar,value) <- newPVar
  result <- tryWritePVar pvar True
  assertBool "Did tryReadPVar return the correct result?" result
  assertBool "Does the PVar have the correct value?" value

tests =
    [   testCase "Create -> Write -> Read" test_create_write_read
    ,   testCase "Create -> Read" test_create_read
    ,   testCase "Create -> Write -> tryRead" test_create_write_tryread
    ,   testCase "Create -> tryRead" test_create_tryread
    ,   testCase "Create -> Write -> Write (fails) -> Read" test_create_write_write_read
    ,   testCase "Create -> Write -> tryWrite -> Read" test_create_write_trywrite_read
    ,   testCase "Create -> tryWrite -> Read" test_create_write_trywrite_read
    ]

main = defaultMain tests
