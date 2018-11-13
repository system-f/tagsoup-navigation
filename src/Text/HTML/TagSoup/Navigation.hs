{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Text.HTML.TagSoup.Navigation where

import Hedgehog(Gen, Property, property, forAll, (===))
import Hedgehog.Function(Arg, Vary, forAllFn, fn)
import qualified Hedgehog.Gen as Gen(list, bool)
import qualified Hedgehog.Range as Range(constant)
import Control.Applicative(Applicative(pure, (<*>)))
import Control.Category((.), id)
import Data.Bifunctor
import Data.Bool(Bool)
import Data.Either(Either)
import Data.Eq(Eq)
import Data.Functor(Functor(fmap))
import Data.Function(($))
import Data.Functor((<$>))
import Data.Functor.Classes(Eq1, Show1)
import Data.Functor.Compose(Compose(Compose))
import Data.Functor.Identity(Identity(Identity))
import Data.Foldable(Foldable(foldMap))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Monoid(mempty, mappend)
import Data.Traversable(Traversable(traverse))
import Prelude(Show)

data NN = -- 99
  NN
  deriving (Eq, Show)

data Four = -- 4
  Four
  deriving (Eq, Show)

data L a =
    LN
  | LC a (L a)
  deriving (Eq, Show)

data A a =
  A a a
  deriving (Eq, Show)
  
data T a =
    TO a (L (A a))
  | TC a
  | TX a
  | TM a
  | TW a
  | TP NN -- 99
  deriving (Eq, Show)
  
data TT a =
    TTB a (L (A a)) (L (TT a))
  | F (T a)
  deriving (Eq, Show)
  
data L' a =
  L' (L a) a (L a)
  deriving (Eq, Show)
  
data A' a =
  A' Bool a
  deriving (Eq, Show)

-- T' a    = L (A a) + a * L (A a) * A' a * L (A a) + 4
-- more usefully for zippers:
-- dT da a = da * L (A a) + a * L (A a) * dA da a * L (A a) + 4 * da
data T' a =
    TO'
      a -- focus
      (L (A a))
  | T'
      a -- focus
      (L (A a))
      (A' a)
      (L (A a))
  | TCTMW
      Four -- 4
      a -- focus

-- TT' a = L (A a) * L (TT a) + a * L (A a) * A' a * L (A a) * L (TT a) + a * L (A a) * L (TT a) * TT' a * L (TT a) + T' a

-- more usefully for zippers:
-- dTT da a = (da * L (A a) * L (TT a)) + (a * L (A a) * dA da a * L (A a) * L (TT a)) + (a * L (A a) * L (TT a) * dTT da a * L (TT a)) + dT da a

-- dTT in its raw form has a nice linear spine; you can replace it with:
-- dTT_ da a = ((da * L (A a) * L (TT a)) + (a * L (A a) * dA da a * L (A a) * L (TT a)) + dT da a) * L (a * L (A a) * L (TT a) * _ * L (TT a))
-- where the _ doesn't stand for anything, it's just there to let you know how to handle the ordering
-- by "handle the ordering", I mean if you want to match the traversal ordering of the TT-zipper to the underlying TT, then all the stuff on the left of the _ but in those parens has to come before the stuff in the sum on the far left (and in reverse order), and the stuff to the right of the _ comes last (in the expected order)

data TT2 a =
  TT2
    a
    (L (A a))
    (A' a)
    (L (A a))
    (L (TT a))
  deriving (Eq, Show)

data TT' a =
    TT
      a
      (L (A a))
      (L (TT a))
  | TT2'
      (Either a (TT2 a))
      (L (a, L (A a)))
      (L (TT a))
      (L (TT a))
  deriving (Eq, Show)

----

-- the thing is then to write the traversable instances for both, and the comonad instances for the zippers, and the rezips for the zippers, and do some quickcheck tests
-- for things like the comonad laws, that toList (rezip x) = toList x, etc

-- Functor

instance Functor L where
  fmap _ LN =
    LN
  fmap f (LC h t) =
    LC (f h) (fmap f t)

instance Functor L' where
  fmap f (L' l x r) =
    L' (fmap f l) (f x) (fmap f r)

instance Functor A where
  fmap f (A a1 a2) =
    A (f a1) (f a2)

instance Functor A' where
  fmap f (A' b a) =
    A' b (f a)

instance Functor T where
  fmap f (TO a x) =
    TO (f a) (fmap (fmap f) x)
  fmap f (TC a) =
    TC (f a)
  fmap f (TX a) =
    TX (f a)
  fmap f (TM a) =
    TM (f a)
  fmap f (TW a) =
    TW (f a)
  fmap _ (TP nn) =
    TP nn

instance Functor T' where
  fmap f (TO' x w) =
    TO' (f x) (fmap (fmap f) w)
  fmap f (T' x l a r) =
    T' (f x) (fmap (fmap f) l) (fmap f a) (fmap (fmap f) r)
  fmap f (TCTMW four x) =
    TCTMW four (f x)

instance Functor TT where
  fmap f (TTB a x y) =
    TTB (f a) (fmap (fmap f) x) (fmap (fmap f) y)
  fmap f (F x) =
    F (fmap f x)

instance Functor TT2 where
  fmap f (TT2 x p q r s) =
    TT2 (f x) (fmap (fmap f) p) (fmap f q) (fmap (fmap f) r) (fmap (fmap f) s)

instance Functor TT' where
  fmap f (TT a x y) =
    TT (f a) (fmap (fmap f) x) (fmap (fmap f) y)
  fmap f (TT2' p q r s) =
    TT2' (bimap f (fmap f) p) (fmap (bimap f (fmap (fmap f))) q)  (fmap (fmap f) r) (fmap (fmap f) s)
    
-- Foldable

instance Foldable L where
  foldMap _ LN =
    mempty
  foldMap f (LC h t) =
    f h `mappend` foldMap f t

instance Foldable L' where
  foldMap f (L' l x r) =
    foldMap f l `mappend` f x `mappend` foldMap f r

instance Foldable A where
  foldMap f (A a1 a2) =
    f a1 `mappend` f a2

instance Foldable A' where
  foldMap f (A' _ a) =
    f a

instance Foldable T where
  foldMap f (TO a x) =
    f a `mappend` foldMap (foldMap f) x
  foldMap f (TC a) =
    f a
  foldMap f (TX a) =
    f a
  foldMap f (TM a) =
    f a
  foldMap f (TW a) =
    f a
  foldMap _ (TP _) =
    mempty

instance Foldable T' where
  foldMap f (TO' x w) =
    f x `mappend` foldMap (foldMap f) w
  foldMap f (T' x l a r) =
    f x `mappend` foldMap (foldMap f) l `mappend` foldMap f a `mappend` foldMap (foldMap f
      ) r
  foldMap f (TCTMW _ x) =
    f x

-- Traversable

lawNaturalityTraversable ::
  forall f a b.
  (Show a, Show b, Arg a, Vary a, Show (f a), Show (f b), Eq (f (f b)), Show (f (f b)), Traversable f, Applicative f) =>
  (forall z. f z -> f z)
  -> (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Gen b
  -> Property
lawNaturalityTraversable t genF genA genB =
  property $
    do  f <- forAllFn $ fn @a (genF genB)
        x <- forAll $ genF genA
        let p = traverse (t . f) x
        let q = (t . traverse f) x
        p === q

prop_lawNaturalityTraversable ::
  Property
prop_lawNaturalityTraversable =
  lawNaturalityTraversable
    id -- todo
    (Gen.list (Range.constant 0 5))
    Gen.bool
    Gen.bool

lawIdentityTraversable ::
  forall f a.
  (Eq (f a), Show (f a), Traversable f) =>
  (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Property
lawIdentityTraversable genF genA =
  property $
    do  x <- forAll $ genF genA
        let p = traverse Identity x
        let q = Identity x
        p === q
        
prop_lawIdentityTraversable ::
  Property
prop_lawIdentityTraversable =
  lawIdentityTraversable
    (Gen.list (Range.constant 0 5))
    Gen.bool

lawCompositionTraversable ::
  forall f a b c.
  ( Show (f a)
    , Show (f b)
    , Show a, Arg a, Vary a
    , Show b, Arg b, Vary b
    , Eq (f c)
    , Eq1 f
    , Show1 f
    , Show (f c)
    , Traversable f
    , Applicative f
  ) =>
  (forall x. Gen x -> Gen (f x))
  -> Gen a
  -> Gen b
  -> Gen c
  -> Property
lawCompositionTraversable genF genA genB genC =
  property $ do
    f <- forAllFn $ fn @a (genF genB)
    g <- forAllFn $ fn @b (genF genC)
    x <- forAll $ genF genA
    let p = traverse (Compose . fmap g . f) x
    let q = (Compose . fmap (traverse g) . traverse f) x
    p === q

prop_lawCompositionTraversable ::
  Property
prop_lawCompositionTraversable =
  lawCompositionTraversable
    (Gen.list (Range.constant 0 5))
    Gen.bool
    Gen.bool
    Gen.bool

instance Traversable L where
  traverse _ LN =
    pure LN
  traverse f (LC h t) =
    LC <$> f h <*> traverse f t

instance Traversable L' where
  traverse f (L' l x r) =
    L' <$> traverse f l <*> f x <*> traverse f r

instance Traversable A where
  traverse f (A a1 a2) =
    A <$> f a1 <*> f a2
    
instance Traversable A' where
  traverse f (A' b a) =
    A' b <$> f a

instance Traversable T where
  traverse f (TO a x) =
    TO <$> f a <*> traverse (traverse f) x
  traverse f (TC a) =
    TC <$> f a
  traverse f (TX a) =
    TX <$> f a
  traverse f (TM a) =
    TM <$> f a
  traverse f (TW a) =
    TW <$> f a
  traverse _ (TP nn) =
    pure (TP nn)

instance Traversable T' where
  traverse f (TO' x w) =
    TO' <$> f x <*> traverse (traverse f) w
  traverse f (T' x l a r) =
    T' <$> f x <*> traverse (traverse f) l <*> traverse f a <*> traverse (traverse f) r
  traverse f (TCTMW four x) =
    TCTMW four <$> f x

-- Comonad

instance Comonad L' where
  extract (L' _ x _) =
    x
  duplicate z =
    let unfoldr f x =
          case f x of
            Just (h, r) ->
              LC h (unfoldr f r)
            Nothing ->
              LN
        moveL (L' LN _ _) =
          Nothing
        moveL (L' (LC h t) x r) =
          Just (L' t h (LC x r))
        moveR (L' _ _ LN) =
          Nothing
        moveR (L' l x (LC h t)) =
          Just (L' (LC x l) h t)
        dup x =
          (x, x)
        unf m =
          unfoldr (fmap dup . m) z
    in  L' (unf moveL) z (unf moveR)

instance Comonad A' where
  extract (A' _ a) =
    a
  duplicate (A' b a) =
    A' b (A' b a)

----

class Functor f => Comonad f where
  extract ::
    f a
    -> a
  duplicate ::
    f a
    -> f (f a)
