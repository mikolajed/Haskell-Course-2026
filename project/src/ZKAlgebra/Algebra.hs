-- |
-- Module      : ZKAlgebra.Algebra
-- Description : Algebraic typeclass hierarchy
--
-- Pure mathematical definitions for the algebraic structures used
-- throughout the library. No concrete types — just the laws.
--
-- @
--   Group  ⊃  Ring  ⊃  Field  ⊃  FiniteField
-- @
module ZKAlgebra.Algebra
  ( -- * Algebraic hierarchy
    Group (..),
    Ring (..),
    Field (..),
    FiniteField (..),
  )
where

-------------------------------------------------------------------------------
-- Algebraic hierarchy
-------------------------------------------------------------------------------

-- | A group: a set with a single binary operation, an identity element,
-- and inverses.
--
-- Laws:
--
-- * @gOp (gOp a b) c == gOp a (gOp b c)@   (associativity)
-- * @gOp a gIdentity == a@                  (right identity)
-- * @gOp gIdentity a == a@                  (left identity)
-- * @gOp a (gInverse a) == gIdentity@       (right inverse)
class (Eq a, Show a) => Group a where
  -- | The group operation.
  gOp :: a -> a -> a

  -- | The identity element.
  gIdentity :: a

  -- | The inverse of an element.
  gInverse :: a -> a

-- | A ring: a set with addition, multiplication, and additive inverse.
--
-- Laws:
--
-- * @(a + b) + c == a + (b + c)@     (additive associativity)
-- * @a + b == b + a@                  (additive commutativity)
-- * @a + 0 == a@                      (additive identity)
-- * @a + negate a == 0@               (additive inverse)
-- * @(a * b) * c == a * (b * c)@     (multiplicative associativity)
-- * @a * 1 == a@                      (multiplicative identity)
-- * @a * (b + c) == a*b + a*c@       (distributivity)
class (Eq a, Show a, Num a) => Ring a

-- | A field: a ring where every nonzero element has a multiplicative inverse.
--
-- Additional law:
--
-- * @a /= 0  ==>  a * fInv a == 1@   (multiplicative inverse)
class (Ring a) => Field a where
  -- | Multiplicative inverse. Partial: undefined for zero.
  fInv :: a -> a

  -- | Field division: @fDiv a b = a * fInv b@.
  fDiv :: a -> a -> a
  fDiv a b = a * fInv b

-- | A finite field with a known number of elements.
class (Field a) => FiniteField a where
  -- | The order (number of elements) of the field.
  fieldOrder :: proxy a -> Integer
