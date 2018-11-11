{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.HTML.TagSoup.Navigation.Zipper.Parent(
  Parent(..)
, AsParent(..)
, HasParent(..)
, parentSiblingsLTagTree
, parentAttributesAttribute
, parentSiblingsRTagTree
, parentSiblingsTagTree
) where

import Control.Applicative(Applicative((<*>), pure))
import Control.Category(id, (.))
import Control.Lens(Reversing(reversing), Each(each), Prism', Lens', Traversal')
import Data.Eq(Eq)
import Data.Eq.Deriving(deriveEq1)
import Data.Ord.Deriving(deriveOrd1)
import Data.Foldable(Foldable(foldMap))
import Data.Functor(Functor(fmap), (<$>))
import Data.Monoid(mappend)
import Data.Ord(Ord)
import Data.Semigroup(Semigroup((<>)))
import Data.Traversable(Traversable(traverse))
import Prelude(Show)
import Text.HTML.TagSoup.Navigation.Types.Attribute(Attribute)
import Text.HTML.TagSoup.Navigation.Types.TagTree(TagTree)
import Text.Show.Deriving(deriveShow1)

data Parent str =
  Parent
    [TagTree str]
    str
    [Attribute str]
    [TagTree str]
  deriving (Eq, Ord, Show)

instance Functor Parent where
  fmap f (Parent l x a r) =
    Parent (fmap (fmap f) l) (f x) (fmap (fmap f) a) (fmap (fmap f) r)

instance Foldable Parent where
  foldMap f (Parent l x a r) =
    foldMap (foldMap f) l `mappend` f x `mappend` foldMap (foldMap f) a `mappend` foldMap (foldMap f) r

-- todo Foldable1

instance Traversable Parent where
  traverse f (Parent l x a r) =
    Parent <$> traverse (traverse f) l <*> f x <*> traverse (traverse f) a <*> traverse (traverse f) r

-- todo Traversable1

instance Semigroup str => Semigroup (Parent str) where
  Parent l1 x1 a1 r1 <> Parent l2 x2 a2 r2 =
    Parent (l1 <> l2) (x1 <> x2) (a1 <> a2) (r1 <> r2)

instance Reversing (Parent str) where
  reversing (Parent l x a r) =
    Parent (reversing l) x (reversing a) (reversing r)

instance Each (Parent str) (Parent str') str str' where
  each =
    traverse
  
class AsParent s str | s -> str where
  _Parent ::
    Prism' s (Parent str)

instance AsParent (Parent str) str where
  _Parent =
    id

class HasParent s str | s -> str where
  parent ::
    Lens' s (Parent str)
  parentSiblingsL ::
    Lens' s [TagTree str]
  parentSiblingsL =
    parent . parentSiblingsL
  parentFocus ::
    Lens' s str
  parentFocus =
    parent . parentFocus
  parentAttributes ::
    Lens' s [Attribute str]
  parentSiblingsR ::
    Lens' s [TagTree str]
  parentSiblingsR =
    parent . parentSiblingsR

instance HasParent (Parent str) str where
  parent =
    id
  parentSiblingsL f (Parent l x a r) =
    fmap (\l' -> Parent l' x a r) (f l)
  parentFocus f (Parent l x a r) =
    fmap (\x' -> Parent l x' a r) (f x)
  parentAttributes f (Parent l x a r) =
    fmap (\a' -> Parent l x a' r) (f a)
  parentSiblingsR f (Parent l x a r) =
    fmap (\r' -> Parent l x a r') (f r)

parentSiblingsLTagTree ::
  HasParent a str =>
  Traversal' a (TagTree str)
parentSiblingsLTagTree =
  parentSiblingsL . traverse

parentAttributesAttribute ::
  HasParent a str =>
  Traversal' a (Attribute str)
parentAttributesAttribute =
  parentAttributes . traverse

parentSiblingsRTagTree ::
  HasParent a str =>
  Traversal' a (TagTree str)
parentSiblingsRTagTree =
  parentSiblingsR . traverse

parentSiblingsTagTree ::
  HasParent a str =>
  Traversal' a (TagTree str)
parentSiblingsTagTree =
  parent . (\f (Parent l x a r) -> Parent <$> traverse f l <*> pure x <*> pure a <*> traverse f r)

deriveEq1 ''Parent
deriveOrd1 ''Parent
deriveShow1 ''Parent
