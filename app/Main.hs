module Main where

import Control.Monad.Except

data Error
  = WrongPassword
  | WrongEmail
  | WrongCaptcha
  deriving (Show)

main :: IO ()
main = do
  result <- runExceptT checkManyThings
  case result of
    Right () -> putStrLn "Logged in"
    Left WrongPassword -> putStrLn "wrong password!"
    Left WrongEmail -> putStrLn "wrong email!"
    Left WrongCaptcha -> putStrLn "wrong email!"

checkManyThings :: ExceptT Error IO ()
checkManyThings = do
  checkEmail
  checkPassword
  checkCaptcha

checkPassword :: ExceptT Error IO ()
checkPassword = do
  liftIO . putStrLn $ "Enter your password:"
  password <- liftIO getLine
  if password == "abc" then
    return ()
  else
    throwError WrongPassword

checkEmail :: ExceptT Error IO ()
checkEmail = do
  liftIO . putStrLn $ "Enter your email:"
  email <- liftIO getLine
  if email == "john@example.com" then
    return ()
  else
    throwError WrongEmail

checkCaptcha :: ExceptT Error IO ()
checkCaptcha = do
  liftIO . putStrLn $ "Enter the captcha:"
  captcha <- liftIO getLine
  if captcha == "12345" then
    return ()
  else
    throwError WrongCaptcha
