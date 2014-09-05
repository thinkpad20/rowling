{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Rowling.Common (
    module ClassyPrelude,
    module Control.Applicative,
    module Control.Exception.ErrorList,
    module Control.Monad,
    module Control.Monad.Except,
    module Control.Monad.Identity,
    module Control.Monad.State.Strict,
    module Control.Monad.Reader,
    module Control.Monad.Trans,
    module Data.Char,
    module Data.Default,
    module Data.HashMap.Strict,
    module Data.Maybe,
    module GHC.Exts,
    module Text.Render,
    Name, for, tuple
  ) where

import ClassyPrelude hiding (assert, asList, find)
import qualified Prelude as P
import Control.Exception.ErrorList
import Control.Monad (when)
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Monad.Reader (ReaderT(..), MonadReader(..), (<=<), (>=>), ask,
                             asks)
import Control.Monad.State.Strict (MonadState, StateT, State, get, gets,
                                   modify, put, liftM, liftIO, runState,
                                   runStateT, execState, execStateT, evalState,
                                   evalStateT)
import Control.Monad.Except (ExceptT, MonadError(..), throwError, runExceptT)
import Control.Monad.Identity (Identity(..))
import Control.Applicative hiding (empty)
import Data.Char (isDigit)
import Data.Default
import Data.HashMap.Strict (HashMap, keys, (!))
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import qualified Data.Text as T
import GHC.Exts (IsList)
import Text.Render

type Name = Text

-- | Map reversed.
for :: Functor f => f a -> (a -> b) -> f b
for = flip map

-- | Takes two applicative actions and returns their result as a 2-tuple.
tuple :: Applicative f => f a -> f b -> f (a, b)
tuple action1 action2 = (,) <$> action1 <*> action2
