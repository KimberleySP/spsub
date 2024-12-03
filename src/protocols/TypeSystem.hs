{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Protocols.TypeSystem where

import Data.Kind
import GHC.TypeLits

-- | Protocol type family forms a category where:
-- Objects are protocol types
-- Morphisms are protocol transformations
data Protocol :: Type -> Type where
  -- | Initial object in the category
  Init   :: Protocol ()
  -- | Terminal object
  Term   :: Protocol Void
  -- | Product of protocols
  Prod   :: Protocol a -> Protocol b -> Protocol (a, b)
  -- | Coproduct of protocols
  Coprod :: Protocol a -> Protocol b -> Protocol (Either a b)

-- | Natural transformation between protocols
-- Satisfies: ∀f,g. τ ∘ F(f) = G(f) ∘ τ
class ProtocolMorphism (f :: Type -> Type) (g :: Type -> Type) where
  transform :: f a -> g a

-- | Protocol composition forms a monoid:
-- Identity: id ∘ p = p ∘ id = p
-- Associativity: (p ∘ q) ∘ r = p ∘ (q ∘ r)
compose :: Protocol a -> Protocol b -> Protocol (a -> b) 