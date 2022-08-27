module ExceptionHandling (isBelow10, isBelow10R, catchFire, ServerException (..), runAndCatch) where

import Control.Exception (Exception, catch, throw)
import GHC.IO (catchAny)

{-
Three types of exceptions exist:
    - impure exceptions: exception thrown inside a pure context!
    - synchronous exceptions: exception generated by the current thread
    - asynchronous exceptions: exception generated by another thread or the runtime system
-}

-- Impure exception
isBelow10 :: Int -> Bool
isBelow10 n = (n < 10) || error "above or equal to 10!"

isBelow10R :: Int -> Either String ()
isBelow10R n = if n < 10 then Right () else Left (error "above or equal to 10!")

data ServerException
    = ServerOnFireException
    | ServerNotPluggedInException
    deriving (Show, Eq)
instance Exception ServerException

data MyException
    = ThisException
    | ThatException
    deriving (Show)
instance Exception MyException

runAndCatch :: IO () -> IO ()
runAndCatch action =
    action
        `catchAny` print
        `catch` (\e -> print (e :: ServerException))
        `catch` (\e -> print (e :: MyException))

catchFire :: a
catchFire =
    throw ServerOnFireException