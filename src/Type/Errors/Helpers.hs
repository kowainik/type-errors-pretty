{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
Copyright:  (c) 2019-2020 Dmitrii Kovanikov
            (c) 2020-2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Dmitrii Kovanikov <kovanikov@gmail.com>
            Kowainik <xrom.xkov@gmail.com>

This module provides type-level helpers for operations on type-level lists.
-}
module Type.Errors.Helpers
       ( -- * Helper type families
         Intercalate,
       ) where

import GHC.TypeLits

-- | Internal type family to prepend all the elements of a list, used to
-- construct `Intersperse`.
type family PrependToAll (sep :: a) (xs :: [a]) :: [a] where
  PrependToAll _ '[] = '[]
  PrependToAll sep (x ': xs) = sep ': x ': PrependToAll sep xs

-- | Internal type family to intersperse a list with a given seperator,
-- type-level version of `Data.List.intersperse`.
type family Intersperse (sep :: a) (xs :: [a]) :: [a] where
  Intersperse _ '[] = '[]
  Intersperse sep (x ': xs) = x ': PrependToAll sep xs

-- | Internal type family to concatenate all the type-level symbols in a list.
type family Concat (xs :: [Symbol]) :: Symbol where
  Concat '[] = ""
  Concat (x:xs) = AppendSymbol x (Concat xs)

{- | Type family to insert a symbol in between a given list of symbols and concatenate.

>>> :kind! Intercalate ", " '["Int", "String", "Bool"]
Intercalate ", " '["Int", "String", "Bool"] :: Symbol
= "Int, String, Bool"
-}
type family Intercalate (x :: Symbol) (xs :: [Symbol]) :: Symbol where
  Intercalate x xs = Concat (Intersperse x xs)
