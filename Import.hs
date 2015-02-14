module Import
    ( module Import
    ) where

import           Prelude               as Import hiding (head, init, last,
                                                         readFile, tail,
                                                         writeFile, FilePath)
import           Yesod                 as Import hiding
  ( Route (..)
  , selectSource
  , count
  , Value
  , delete
  , update
  , (==.)
  , (!=.)
  , (*=.)
  , (+=.)
  , (-=.)
  , (/=.)
  , (<.)
  , (<=.)
  , (=.)
  , (>.)
  , (>=.)
  , (||.)
  )

import           Control.Applicative   as Import (pure, (<$>), (<*>), (<*))
import           Control.Monad         as Import (liftM)
import           Data.Maybe            as Import (maybeToList, isJust)
import           Data.Text             as Import (Text)
import           Database.Esqueleto    as Import
import           Text.Markdown         as Import

import           Foundation            as Import
import           Model                 as Import
import           Settings              as Import
import           Settings.Development  as Import
import           Settings.StaticFiles  as Import
import           Yesod.Auth            as Import
import           Yesod.Form.Bootstrap3 as Import
import           Yesod.Text.Markdown   as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid           as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid           as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

required :: FieldView app -> Text
required fv = case fvRequired fv of
  True -> "required"
  False -> "optional"

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (x, y) = (x, f y)

mapSnd3 :: (a -> b) -> (c, a, d) -> (c, b, d)
mapSnd3 f (x, y, z) = (x, f y, z)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
