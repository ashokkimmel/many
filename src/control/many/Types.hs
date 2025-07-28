{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Many.Types where
module Types where 
import GHC.TypeNats
data Zero = Zero
  deriving (Show)
newtype Succ x = Succ x
  deriving (Show)
type family ToPeano n where
  ToPeano 0 = Zero
  ToPeano n = Succ (ToPeano (n-1))
