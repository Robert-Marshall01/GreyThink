{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      : Grey.Error.Result
-- Description : Result type for Grey SDK operations
-- License     : MIT
--
-- A Result type representing success or failure, similar to Either
-- but specialized for GreyError.

module Grey.Error.Result
  ( -- * Result Type
    Result(..)
    -- * Constructors
  , ok
  , err
    -- * Pattern Matching
  , isOk
  , isErr
    -- * Extractors
  , fromResult
  , getOr
  , getOrThrow
    -- * Transformations
  , mapResult
  , mapError
  , flatMap
  , partition
    -- * Combinators
  , sequence_
  , traverse_
  ) where

import Control.Exception (throw, Exception)
import GHC.Generics (Generic)
import Grey.Error.GreyError (GreyError)

-- | Result type representing success or failure.
data Result a
  = Ok a           -- ^ Successful result with value
  | Err GreyError  -- ^ Failed result with error
  deriving (Eq, Show, Generic, Functor)

instance Applicative Result where
  pure = Ok
  Ok f <*> Ok a = Ok (f a)
  Ok _ <*> Err e = Err e
  Err e <*> _ = Err e

instance Monad Result where
  Ok a >>= f = f a
  Err e >>= _ = Err e

-- | Create a successful result.
ok :: a -> Result a
ok = Ok

-- | Create a failed result.
err :: GreyError -> Result a
err = Err

-- | Check if the result is a success.
isOk :: Result a -> Bool
isOk (Ok _) = True
isOk (Err _) = False

-- | Check if the result is a failure.
isErr :: Result a -> Bool
isErr = not . isOk

-- | Extract value from result using handlers.
fromResult :: (GreyError -> b) -> (a -> b) -> Result a -> b
fromResult onErr onOk result = case result of
  Ok a -> onOk a
  Err e -> onErr e

-- | Get the value or a default.
getOr :: a -> Result a -> a
getOr def result = case result of
  Ok a -> a
  Err _ -> def

-- | Exception wrapper for throwing GreyError.
newtype GreyException = GreyException GreyError
  deriving (Show)

instance Exception GreyException

-- | Get the value or throw the error as an exception.
-- Note: Use with caution in pure code.
getOrThrow :: Result a -> a
getOrThrow result = case result of
  Ok a -> a
  Err e -> throw (GreyException e)

-- | Map over the success value.
mapResult :: (a -> b) -> Result a -> Result b
mapResult = fmap

-- | Map over the error.
mapError :: (GreyError -> GreyError) -> Result a -> Result a
mapError f result = case result of
  Ok a -> Ok a
  Err e -> Err (f e)

-- | Flat map (bind) over the result.
flatMap :: (a -> Result b) -> Result a -> Result b
flatMap = (=<<)

-- | Partition a list of results into errors and successes.
partition :: [Result a] -> ([GreyError], [a])
partition = foldr go ([], [])
  where
    go (Ok a) (errs, oks) = (errs, a : oks)
    go (Err e) (errs, oks) = (e : errs, oks)

-- | Sequence results, returning first error or unit.
sequence_ :: [Result ()] -> Result ()
sequence_ [] = Ok ()
sequence_ (Ok _ : rest) = Grey.Error.Result.sequence_ rest
sequence_ (Err e : _) = Err e

-- | Traverse with a function, returning first error or unit.
traverse_ :: (a -> Result ()) -> [a] -> Result ()
traverse_ f = Grey.Error.Result.sequence_ . map f
