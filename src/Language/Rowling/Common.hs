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
    Render(..), ErrorList(..), FuzzyEquals(..), Name,
    throwError1, throwErrorC, addErrorC, assert, assertC, wrapJust,
    withHandler, ifErrorDo, ifErrorReturn, for, showError, showError',
    renderTicks, errorC, tuple
  ) where

import ClassyPrelude hiding (assert, asList, find)
import qualified Prelude as P
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
import Text.Parsec (ParseError)

type Name = Text
data ErrorList = EL [Text] deriving (Show, Eq)

-- | A class for pretty printing, and in general, for "showing" as a `Text`.
class Show a => Render a where
  -- | Render the object as a `Text`.
  render :: a -> Text
  render = pack . show
  -- | Many types of objects need to be rendered in parentheses.
  renderParens :: a -> Text
  renderParens = render
  -- | Render in the `IO` monad.
  renderIO :: MonadIO m => a -> m Text
  renderIO = return . render

instance Render Int
instance Render Bool
instance Render Integer
instance Render Double
instance Render Text
instance Render ParseError
instance (Render a, Render b) => Render (a, b) where
  render (a, b) = "(" <> render a <> ", " <> render b <> ")"
instance Render a => Render [a] where
  render list = "[" <> intercalate ", " (map render list) <> "]"
instance Render ErrorList where
  render (EL msgs) = T.unlines $ "Error:" : map (sp <>) msgs where
    sp = T.replicate 2 " "

-- | For fuzzy equality, particularly in types. Note that directionality is
-- important. For example, with content types, @GenericList ~~ ListOf x@
-- (for all @x@), but not the other way around.
class Eq a => FuzzyEquals a where
  (~~) :: a -> a -> Bool
  (~~) = (==)

instance (Eq k, Hashable k, FuzzyEquals a) => FuzzyEquals (HashMap k a) where
  map1 ~~ map2 = go $ H.toList map1 where
    go [] = True
    go ((field, typ):rest) = case H.lookup field map1 of
      -- If the type doesn't exist in the second map, they're not compatible.
      Nothing -> False
      -- Otherwise, they must be compatible.
      Just typ' -> typ ~~ typ' && go rest

-- | Renders and surrounds in backticks. Useful for printing user input.
renderTicks :: Render a => a -> Text
renderTicks x = "`" <> render x <> "`"

-- | Throws a single string as an error.
throwError1 :: MonadError ErrorList m => Text -> m a
throwError1 msg = throwError' [msg]

-- | Throws a list of strings as an error.
throwError' :: MonadError ErrorList m => [Text] -> m a
throwError' = throwError . EL

-- | Concatenates a list of strings and throws them as an error.
throwErrorC :: MonadError ErrorList m => [Text] -> m a
throwErrorC = throwError1 . mconcat

-- | Throws a new error with the given string added on.
addError1 :: MonadError ErrorList m => Text -> ErrorList -> m a
addError1 newMsg (EL msgs) = throwError $ EL $ msgs <> [newMsg]

-- | Throws a new error with the concatenation of the argument added on.
addErrorC :: MonadError ErrorList m => [Text] -> ErrorList -> m a
addErrorC list = addError1 (mconcat list)

-- | Useful when the handler is more concise than the action.
withHandler :: MonadError e m => (e -> m a) -> m a -> m a
withHandler = flip catchError

-- | Useful when the handler is more concise than the action.
wrapJust :: MonadError e m => m a -> m (Maybe a)
wrapJust action = liftM Just action `ifErrorReturn` Nothing

-- | Performs an action, returning the second argument if it fails.
ifErrorReturn :: MonadError e m => m a -> a -> m a
ifErrorReturn action a = action `ifErrorDo` return a

-- | Specifies what to do if the given action fails.
ifErrorDo :: MonadError e m => m a -> m a -> m a
ifErrorDo action action' = action `catchError` \_ -> action'

-- | If the test is false, throws an error with the given message.
assert :: MonadError ErrorList m => Bool -> Text -> m ()
assert True _ = return ()
assert False msg = throwError1 msg

-- | Same as `assert`, but concatenates a text list.
assertC :: MonadError ErrorList m => Bool -> [Text] -> m ()
assertC test = assert test . mconcat

-- | Pretty-prints errors that use `Either`.
showError :: (Render e, Render b) => (a -> Either e b) -> a -> IO ()
showError func arg = case func arg of
  Left e -> P.putStrLn $ unpack $ render e
  Right x -> P.putStrLn $ unpack $ render x

-- | Pretty-prints errors that use `Either`, appearing in tuples.
showError' :: (Render e, Render b) => (a -> (Either e b, c)) -> a -> IO ()
showError' func arg = case func arg of
  (Left e, _) -> P.putStrLn $ unpack $ render e
  (Right x, _) -> P.putStrLn $ unpack $ render x

-- | Concatenates a list of `Text` and makes an error out of it.
errorC :: [Text] -> a
errorC = P.error . unpack . mconcat

-- | Map reversed.
for :: Functor f => f a -> (a -> b) -> f b
for = flip map

-- | Takes two applicative actions and returns their result as a 2-tuple.
tuple :: Applicative f => f a -> f b -> f (a, b)
tuple action1 action2 = (,) <$> action1 <*> action2
