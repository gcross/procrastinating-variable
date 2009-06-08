|
Module      : Data.PVar
Copyright   : (c) 2009 Gregory Crosswhite
License     : BSD3

Maintainer  : Gregory Crosswhite <gcross@phys.washington.edu>
Stability   : experimental
Portability : ghc

Procrastinating variables ('PVar's) are meant to be used in cases where
you want to give someone a value that you do not have available yet,
but will definitely have ready by the time that they need to use it.

'PVar's have the advantage that you do not make the user of your value
execute some kind of unwrapping routine in order to get access to the
value within.  For example, this is useful when you are constructing
closures that you want to go ahead and construct now even though some
of the values that they contain are not available yet.

'PVar's are implemented with a lazy thunk that reads from
an IORef; before the IORef is written to, it contains "bottom" (an
exception with a descriptive error message) so that an error is raised
in the user code if the variable is accidently accessed before the
value is ready.

NOTE: 'PVar's are modeled closely on the 'IVar' implementation in the
       ivar-simple package.  The major difference is that if you try
       to read an IVar before it has been given a value, it blocks
       until the value is available, whereas reading from a 'PVar'
       before it is ready raises an exception.  The reason behind the
       different symantics for 'PVar' is because if the user accidently
       accesses the value too early, you want there to be a lot of
       noise to let him or her know about it, rather than merely
       blocking the thread indefinitely and causing them to wonder
       what went wrong.

> {-# LANGUAGE DeriveDataTypeable #-}

> module Data.PVar
>     ( AccessedTooEarly
>     , AlreadyHasAValue
>     , newPVar
>     , newPVarWithCustomMessage
>     , tryReadPVar
>     , writePVar
>     , tryWritePVar
>     ) where
> import Control.Concurrent.MVar
> import Control.Exception
> import Control.Monad
> import Data.IORef
> import Data.Typeable
> import System.IO.Unsafe

| A procrastinating variable ('PVar' for short)

> data PVar a = PVar !(MVar ()) !(IORef a)

| The exception raised when a 'PVar' is accessed before it is ready.

> data AccessedTooEarly = AccessedTooEarly String deriving (Show, Typeable)
> instance Exception AccessedTooEarly

| The exception raised when one attempts to write to a procrastinating
  variable after it has already been given a value.

> data AlreadyHasAValue = AlreadyHasAValue deriving (Show, Typeable)
> instance Exception AlreadyHasAValue

| Creates a new, empty 'PVar', and returns both a reference you can
  use to fill the value later as well as

> newPVar :: IO (PVar a, a)
> newPVar = newPVarWithCustomMessage "This procrastinating variable was accessed before it was ready."

| Creates a new, empty 'PVar' that raises an exception with a custom
  message.  (Use this if you want to make explicit to the user of this
  variable exactly when they should expect its value to become
  available.)

> newPVarWithCustomMessage :: String -> IO (PVar a, a)
> newPVarWithCustomMessage message = do
>     lock <- newMVar ()
>     ref <- newIORef . throw . AccessedTooEarly $ message
>     let {-# NOINLINE value #-}
>         value = unsafePerformIO $ readIORef ref
>     return (PVar lock ref,value)

| Try to read a procrastinating variable. Returns 'Nothing' if the
  value is not ready yet.

> tryReadPVar :: PVar a -> IO (Maybe a)
> tryReadPVar (PVar lock ref) = block $ do
>     is_empty <- isEmptyMVar lock
>     if is_empty
>         then readIORef ref >>= return . Just
>         else return Nothing

| Writes a value to a 'PVar'.  Raises a 'IVar'. Raises a
  'AlreadyHasAValue' exception if the 'PVar' already has a value.

> writePVar :: PVar a -> a -> IO ()
> writePVar pvar value = do
>     result <- tryWritePVar pvar value
>     unless result $ throwIO AlreadyHasAValue

| Writes a value to a 'PVar'. Returns 'True' if successful.

> tryWritePVar :: PVar a -> a -> IO Bool
> tryWritePVar (PVar lock ref) value = block $ do
>     a <- tryTakeMVar lock
>     case a of
>         Just _  -> writeIORef ref value >> return True
>         Nothing -> return False
