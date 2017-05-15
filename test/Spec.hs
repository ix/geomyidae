{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Network.Gopher

testFileType :: Assertion
testFileType = do
  one <- fileType "hello.jpg"
  two <- fileType "aklsjakda.txt"
  three <- fileType "index.html"
  four <- fileType "abcdefg.tar.gz"
  assertBool "Invalid service type (1)" (one == 'I')
  assertBool "Invalid service type (2)" (two == '0')
  assertBool "Invalid service type (3)" (three == 'h')
  assertBool "Invalid service type (4)" (four == '9')

-- friends its 4am i cant think of any more tests for this ok
-- i mean ill ask someone tomorrow if i remember but i if i dont
-- and theres one obvious to you send a pr or let it slip please ty
testIsValidPath :: Assertion
testIsValidPath = do
  one <- isValidPath "." "../etc"
  two <- isValidPath "." "/test"
  assertBool "Path referencing lower directory found under current directory." (one == Nothing)
  assertBool "Path referencing valid possible location not found." (two /= Nothing)

main :: IO ()
main = do
  testFileType
  testIsValidPath
