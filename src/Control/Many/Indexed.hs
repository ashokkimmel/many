{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
module Control.Many.Indexed (foldN,foldMapN,mapn,liftAn,list,mzipWithn,zipWithn,constN,foldrN,foldr'N,foldlN,foldl'N,nullN) where
import Control.Monad.Zip
import Data.Foldable
import Data.Semigroup
import Control.Applicative (liftA2)
import Control.Many.Types (ToPeano, Zero, Succ)
class MapN num a b c d | num a -> c , num b -> d, num a d -> b, num b c -> d where
    mapN :: (c -> d) -> a -> b
instance MapN Zero a b a b where
    mapN = id
    {-# INLINE mapN #-}
instance (Functor g, MapN x a b (g e) (g f)) => MapN (Succ x) a b e f where
    mapN = mapN @x . fmap
    {-# INLINE mapN #-}
mapn :: forall n a b c d. (MapN (ToPeano n) a b c d) => (c -> d) -> a -> b
mapn = mapN @(ToPeano n)
{-# INLINE mapn #-}
class Applicative f => LiftN' a f c d | a d c -> f, a f c -> d  where
    liftN' :: c -> d
class Applicative f => LiftN a f c d | a d c -> f, a f c -> d  where
    liftN :: c -> d
instance Applicative f => LiftN Zero f a (f a) where
    liftN = pure
    {-# INLINE liftN #-}
instance Applicative f => LiftN (Succ Zero) f (a->b) (f a-> f b) where
    liftN = fmap
    {-# INLINE liftN #-}
instance (LiftN' a b c d) => LiftN (Succ (Succ a)) b c d where liftN = liftN' @a @b @c @d 
instance Applicative f => LiftN' Zero f (a -> b -> c) (f a -> f b -> f c) where
    liftN' = liftA2 
    {-# INLINE liftN' #-}
instance (Applicative f, LiftN' x f y z, MapN (Succ (Succ x)) z m (f (a -> b)) (f a -> f b)) => LiftN' (Succ x) f y m where
    liftN' = mapN @(Succ (Succ x)) (<*>) . liftN' @x @f @y @z
    {-# INLINE liftN' #-}

liftAn :: forall n f start end. (Applicative f, LiftN (ToPeano n) f start end) => start -> end
liftAn = liftN @(ToPeano n)  -- . (pure @f)
{-# INLINE liftAn #-}


class ListN num a where
    listNp :: a
instance ListN Zero [a] where
    listNp = []
instance (ListN x xs,MapN x xs y [a] [a]) => ListN (Succ x) (a -> y) where
    listNp x = mapN @x @xs (x:) (listNp @x @xs)
list :: forall n a. (ListN (ToPeano n) a) => a
list = listNp @(ToPeano n) @a

-- This will give odd error messages, so be warned
class MonadZip f=> MonadZipn num f a b | num f a -> b where
    mZipWithnp :: (a -> b)
instance MonadZip f => MonadZipn (Succ (Succ Zero)) f (a -> b -> c) (f a -> f b -> f c) where
    mZipWithnp = mzipWith
instance (MonadZip f, MonadZipn x f y z, MapN x z m (f (a -> b)) (f a -> f b)) => MonadZipn (Succ x) f y m where
    mZipWithnp = mapN @x (mzipWith id) . mZipWithnp @x @f @y @z
-- 
mzipWithn :: forall n f a b. (MonadZipn (ToPeano n) f a b) => a -> b
mzipWithn = mZipWithnp @(ToPeano n) @f
zipWithn :: forall n a b. (MonadZipn (ToPeano n) [] a b) => a -> b 
zipWithn = mZipWithnp @(ToPeano n) @[]

class ConstN num a b | num a -> b where 
    constNp :: b -> a
instance ConstN Zero a a where constNp = id 
instance ConstN x a b => (ConstN (Succ x)) (p -> a) b where constNp a = const $ constNp @x @a a 
constN :: forall n a b. (ConstN (ToPeano n) a b) => b -> a
constN = constNp @(ToPeano n) @a @b

class Foldn' num a x | num a -> x where 
    foldrNp  :: (x -> b -> b) -> b -> a -> b
    foldr'Np :: (x -> b -> b) -> b -> a -> b
    foldlNp  :: (b -> x -> b) -> b -> a -> b
    foldl'Np :: (b -> x -> b) -> b -> a -> b
    nullNp   :: a -> Bool
    lengthNp :: a -> Int 
    foldMapNp:: Monoid b => (x -> b) -> a -> b
instance Foldable f => Foldn' Zero a a where 
    foldrNp   = flip 
    foldr'Np  = flip . ($!) 
    foldlNp   = ($ )
    foldl'Np  = ($!) 
    nullNp    = const False  
    lengthNp  = const 1
    foldMapNp = id  
instance (Foldn' num x a,Foldable f) => Foldn' (Succ num) (f x) a where 
    foldrNp   = foldr   . flip . foldrNp   @num 
    foldr'Np  = foldr'  . flip . foldr'Np  @num 
    foldlNp   = foldl   . foldlNp   @num 
    foldl'Np  = foldl'  . foldl'Np  @num 
    nullNp    = all @f (nullNp @num @x @a)   
    lengthNp a= getSum $ foldMap (Sum . lengthNp  @num @x @a) a 
    foldMapNp = foldMap . foldMapNp @num
foldrN  :: forall num x a b.(Foldn' (ToPeano num) a x) => (x -> b -> b) -> b -> a -> b 
foldrN  = foldrNp @(ToPeano num) @a @x 
foldr'N :: forall num x a b.(Foldn' (ToPeano num) a x) => (x -> b -> b) -> b -> a -> b 
foldr'N = foldr'Np @(ToPeano num) @a @x 
foldlN  :: forall num x a b.(Foldn' (ToPeano num) a x) => (b -> x -> b) -> b -> a -> b 
foldlN  = foldlNp @(ToPeano num) @a @x 
foldl'N :: forall num x a b.(Foldn' (ToPeano num) a x) => (b -> x -> b) -> b -> a -> b 
foldl'N = foldl'Np @(ToPeano num) @a @x 
nullN :: forall num x a.(Foldn' (ToPeano num) a x) => a -> Bool 
nullN = nullNp @(ToPeano num) @a @x 
foldMapN :: forall num x a b.(Foldn' (ToPeano num) a x) => Monoid b => (x -> b) -> a -> b
foldMapN = foldMapNp @(ToPeano num) @a @x 
-- Note, FoldN does not work with 0
foldN :: forall num x a p f b.((ToPeano num) ~ (Succ p),Foldn' p a x,x ~ f b, Foldable f, Monoid b) => Monoid b => a -> b
foldN = foldMapNp @p @a @x fold 
