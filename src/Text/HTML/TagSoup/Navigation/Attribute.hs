{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Text.HTML.TagSoup.Navigation.Attribute(
  Attribute(..)
, AsAttribute(..)
, HasAttribute(..)
, Row
, Column
, bothAttributes
) where

import Control.Applicative(Applicative((<*>), pure), liftA2)
import Control.Category((.), id)
import Control.Lens(Each(each), Reversing(reversing), Rewrapped, Wrapped(Unwrapped), _Wrapped', Field1(_1), Field2(_2), Prism', Lens', Traversal, iso)
import Control.Monad(Monad((>>=), return))
import Control.Monad.Zip(MonadZip(mzipWith))
import Data.Bool((&&))
import Data.Eq(Eq)
import Data.Foldable(Foldable(foldMap))
import Data.Functor(Functor(fmap), (<$>))
import Data.Functor.Apply(Apply((<.>)))
import Data.Functor.Bind(Bind((>>-)))
import Data.Functor.Classes(Eq1(liftEq), Ord1(liftCompare), Show1(liftShowsPrec), showsBinaryWith)
import Data.Monoid(Monoid(mappend, mempty))
import Data.Ord(Ord)
import Data.Semigroup.Foldable(Foldable1(foldMap1))
import Data.Semigroup.Traversable(Traversable1(traverse1))
import Data.Semigroup(Semigroup((<>)))
import Data.Traversable(Traversable(traverse))
import Prelude(Show)
import Text.HTML.TagSoup(Row, Column)
import qualified Text.HTML.TagSoup as TagSoup()
import qualified Text.HTML.TagSoup.Tree as TagSoup()

data Attribute str =
  Attribute
    str
    str
  deriving (Eq, Ord, Show)

instance Functor Attribute where
  fmap f (Attribute s1 s2) =
    Attribute (f s1) (f s2)

instance Apply Attribute where
  Attribute s1 s2 <.> Attribute s3 s4 =
    Attribute (s1 s3) (s2 s4)
  
instance Applicative Attribute where
  pure s =
    Attribute s s
  (<*>) =
    (<.>)

instance Bind Attribute where
  Attribute a b >>- f =
    let Attribute a' _ = f a
        Attribute _ b' = f b
    in  Attribute a' b'

instance Monad Attribute where
  return =
    pure
  (>>=) =
    (>>-)

instance Foldable Attribute where
  foldMap f (Attribute s1 s2) = 
    f s1 `mappend` f s2

instance Foldable1 Attribute where
  foldMap1 f (Attribute a b) =
    f a <> f b

instance Traversable Attribute where
  traverse f (Attribute s1 s2) =
    Attribute <$> f s1 <*> f s2

instance Traversable1 Attribute where
  traverse1 f (Attribute s1 s2) =
    Attribute <$> f s1 <.> f s2

instance MonadZip Attribute where
  mzipWith =
    liftA2

instance Semigroup str => Semigroup (Attribute str) where
  Attribute s1 s2 <> Attribute s3 s4 =
    Attribute (s1 <> s3) (s2 <> s4)

instance Monoid str => Monoid (Attribute str) where
  Attribute s1 s2 `mappend` Attribute s3 s4 =
    Attribute (s1 `mappend` s3) (s2 `mappend` s4)
  mempty =
    Attribute mempty mempty

instance Each (Attribute str) (Attribute str) str str where
  each f (Attribute s1 s2) =
    Attribute <$> f s1 <*> f s2

instance Reversing (Attribute str) where
  reversing (Attribute s1 s2) =
    Attribute s2 s1

instance Attribute s ~ str =>
  Rewrapped (Attribute x) str

instance Wrapped (Attribute str) where
  type Unwrapped (Attribute str) =
    (str, str)
  _Wrapped' =
    iso
      (\(Attribute s1 s2) -> (s1, s2))
      (\(s1, s2) -> Attribute s1 s2)

instance Eq1 Attribute where
  liftEq f (Attribute s1 s2) (Attribute s3 s4) =
    f s1 s3 && f s2 s4

instance Ord1 Attribute where
  liftCompare f (Attribute s1 s2) (Attribute s3 s4) =
    f s1 s3 `mappend` f s2 s4

instance Show1 Attribute where
  liftShowsPrec f _ k (Attribute s1 s2) =
    showsBinaryWith f f "Attribute" k s1 s2

instance Field1 (Attribute str) (Attribute str) str str where
  _1 =
    attributeName

instance Field2 (Attribute str) (Attribute str) str str where
  _2 =
    attributeValue

class AsAttribute s str | s -> str where
  _Attribute ::
    Prism' s (Attribute str)

instance AsAttribute (Attribute str) str where
  _Attribute = id

class HasAttribute s str | s -> str where
  attribute ::
    Lens' s (Attribute str)
  attributeName ::
    Lens' s str
  attributeName =
    attribute . attributeName
  attributeValue ::
    Lens' s str
  attributeValue =
    attribute . attributeValue

instance HasAttribute (Attribute str) str where
  attribute =
    id
  attributeName f (Attribute s1 s2) =
    fmap (\s1' -> Attribute s1' s2) (f s1)
  attributeValue f (Attribute s1 s2) =
    fmap (\s2' -> Attribute s1 s2') (f s2)

bothAttributes ::
  Traversal (Attribute str) (Attribute str') str str'
bothAttributes f (Attribute s1 s2) =
  Attribute <$> f s1 <*> f s2
