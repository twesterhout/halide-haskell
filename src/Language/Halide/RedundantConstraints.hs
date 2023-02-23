{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Halide.RedundantConstraints
  ( keepRedundantConstraint

    -- * Convenience re-export
  , Proxy (..)
  ) where

import Data.Proxy

-- | Can be used to silence individual "redundant constraint" warnings
--
-- > foo :: ConstraintUsefulForDebugging => ...
-- > foo =
-- >     ..
-- >   where
-- >     _ = keepRedundantConstraint (Proxy @ConstraintUsefulForDebugging))
--
-- __Note:__ this function is taken from [input-output-hk/ouroboros-network](https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus/src/Ouroboros/Consensus/Util/RedundantConstraints.hs).
keepRedundantConstraint :: c => proxy c -> ()
keepRedundantConstraint _ = ()
