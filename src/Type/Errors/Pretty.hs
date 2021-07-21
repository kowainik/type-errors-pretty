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

This module provides type-level functions and operators to make the job of
writing text of custom type errors easier. The motivation behind using custom
type errors is described in detail in the following blog post:

* [A story told by Type Errors](https://kodimensional.dev/type-errors)

If you want to write the text of a custom error message, you need to use
constructors of the 'ErrorMessage' data type. But this gets messy and
inconvenient pretty quickly. Consider the following examples:

@
__type__ MessageText (e1 :: k) (e2 :: k) (es :: [k]) =
       ''Text' "You require the following two effects from your computation:"
 '':$$:' ''Text' ""
 '':$$:' ''Text' "    '" '':<>:' ''ShowType' e1 '':<>:' ''Text' "' and '" '':<>:' ''ShowType' e2 '':<>:' ''Text' "'"
 '':$$:' ''Text' ""
 '':$$:' ''Text' "However, your monad is capable of performing only the following effects:"
 '':$$:' ''Text' ""
 '':$$:' ''Text' "    " '':<>:' ''ShowType' es
@

Using combinators from this library, you can define error messages in a simpler
way:

@
__type__ MessageText (e1 :: k) (e2 :: k) (es :: [k])
    = "You require the following two effects from your computation:"
    '%' ""
    '%' "    '" 'Type.Errors.Pretty.<>' e1 'Type.Errors.Pretty.<>' "' and '" 'Type.Errors.Pretty.<>' e2 'Type.Errors.Pretty.<>' "'"
    '%' ""
    '%' "However, your monad is capable of performing only the following effects:"
    '%' ""
    '%' "    " 'Type.Errors.Pretty.<>' es
@

If you prefer, you can use unicode operators to contstruct messages:

@
__type__ MessageText (e1 :: k) (e2 :: k) (es :: [k])
    = "You require the following two effects from your computation:"
    '•' ""
    '•' "    '" '⊕' e1 '⊕' "' and '" '⊕' e2 '⊕' "'"
    '•' ""
    '•' "However, your monad is capable of performing only the following effects:"
    '•' ""
    '•' "    " '⊕' es
@
-}

module Type.Errors.Pretty
       ( -- * Combinators
         type (<>)
       , type (⊕)
       , type (%)
       , type (•)

         -- * Reexports from "GHC.TypeLits"
       , TypeError

         -- * Helper type families
       , ToErrorMessage
       , CommaSeparated
       ) where

import Data.Kind
import GHC.TypeLits (AppendSymbol, ErrorMessage (..), Symbol, TypeError)
import GHC.Generics
import Type.Errors.Helpers (Intercalate)

{- | Append two types automatically converting them to corresponding
'ErrorMessage' constructors.

>>> :kind! "Integer values have type: " <> Int
"Integer values have type: " <> Int :: ErrorMessage
= 'Text "Integer values have type: " ':<>: 'ShowType Int
-}
infixl 5 <>
type family (<>) (l :: k1) (r :: k2) :: ErrorMessage where
    l <> r = ToErrorMessage l ':<>: ToErrorMessage r

{- | Unicode version of the 'Type.Errors.Pretty.<>' type-level operator.
-}
infixl 5 ⊕
type (⊕) (l :: k1) (r :: k2) = l <> r

{- | Append two types on top of each other automatically converting them to
corresponding 'ErrorMessage' constructors.

>>> :kind! "Expecting value of type: " % "   " <> Integer
"Expecting value of type: " % "   " <> Integer :: ErrorMessage
= 'Text "Expecting value of type: "
  ':$$: ('Text "   " ':<>: 'ShowType Integer)
-}
infixr 4 %
type family (%) (t :: k1) (b :: k2) :: ErrorMessage where
    t % b = ToErrorMessage t ':$$: ToErrorMessage b

{- | Unicode version of the '%' type-level operator.
-}
infixr 4 •
type (•) (t :: k1) (b :: k2) = t % b

-- | Type family to convert any type to 'ErrorMessage'.
type family ToErrorMessage (t :: k) :: ErrorMessage where
    ToErrorMessage (t :: Symbol) = 'Text t
    ToErrorMessage (t :: ErrorMessage) = t
    ToErrorMessage t = 'ShowType t

-- | Internal type family to go from a `Type` to a `Symbol` representation.
-- The names for types without `Generic` instances have to be defined
-- manually, and we do so for a handful of common types.
type family TypeName (a :: Type) :: Symbol where
  TypeName Char = "Char"
  TypeName String = "String"
  TypeName Int  = "Int"
  TypeName (Maybe a) = AppendSymbol "Maybe " (TypeName a)
  TypeName [a] = AppendSymbol "[" (AppendSymbol (TypeName a) "]")
  TypeName (D1 ('MetaData name _ _ _) _ _) = name
  TypeName a = TypeName (Rep a ())

-- | Internal type family to map over a list of `Type`s while applying
-- `TypeName` to each, returning a list of `Symbol`s.
type family CommaSeparated' (ts :: [Type]) :: [Symbol] where
  CommaSeparated' '[] = '[]
  CommaSeparated' (t ': ts) = TypeName t ': CommaSeparated' ts

{- | Type family to display a list of types separated by commas.

@
__data__ UserDefinedTypeDerivingGeneric = C1 | C2 __deriving__ Generic
@
>>> :kind! CommaSeparated '[Int, String, Maybe Int, [[[Char]]], UserDefinedTypeDerivingGeneric]
CommaSeparated '[Int, String, Maybe Int, [[[Char]]], UserDefinedTypeDerivingGeneric] :: Symbol
= "Int, String, Maybe Int, [[String]], UserDefinedTypeDerivingGeneric"

-}
type family CommaSeparated (ts :: [Type]) :: Symbol where
  CommaSeparated ts = Intercalate ", " (CommaSeparated' ts)
