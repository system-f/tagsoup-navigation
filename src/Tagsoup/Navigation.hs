{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Tagsoup.Navigation where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Functor.Classes
import Control.Applicative
import Control.Monad.Zip
import Data.Semigroup
import Data.Semigroup.Foldable
import qualified Text.HTML.TagSoup as TagSoup(Attribute, Tag, Row, Column)
import qualified Text.HTML.TagSoup.Tree as TagSoup(TagTree(..))
import Text.HTML.TagSoup(Row, Column)

import Prelude

{-
-- Lens' (Attribute str) str
attributeName ::
  Field1 s t a b =>
  Lens s t a b
attributeName =
  _1

-- Lens' (Attribute str) str
attributeValue ::
  Field2 s t a b =>
  Lens s t a b
attributeValue =
  _2

class HasTagTree a str | a -> str where
  tagTree ::
    Lens' a (TagTree str)

instance HasTagTree (TagTree str) str where
  tagTree =
    id

class AsTagTree a str | a -> str where
  _TagTree ::
    Prism' a (TagTree str)
  _TagBranch ::
    Prism' a (str, [Attribute str], [TagTree str])
  _TagBranch =
    _TagTree . _TagBranch
  _TagLeaf ::
    Prism' a (Tag str)
  _TagLeaf =
    _TagTree . _TagLeaf

instance AsTagTree (TagTree str) str where
  _TagTree =
    id
  _TagBranch =
    prism'
      (\(s, as, t) -> TagBranch s as t)
      (\tr ->  case tr of
                TagBranch s as t ->
                  Just (s, as, t)
                TagLeaf _ ->
                  Nothing)
  _TagLeaf =
    prism'
      TagLeaf
      (\tr ->  case tr of
                TagBranch _ _ _ ->
                  Nothing
                TagLeaf x ->
                  Just x)

class HasTag a str | a -> str where
  tag ::
    Lens' a (Tag str)

instance HasTag (Tag str) str where
  tag =
    id

class AsTag a str | a -> str where
  _Tag ::
    Prism' a (Tag str)
  _TagOpen ::
    Prism' a (str, [Attribute str])
  _TagOpen =
    _Tag . _TagOpen
  _TagClose ::
    Prism' a str
  _TagClose =
    _Tag . _TagClose
  _TagText ::
    Prism' a str
  _TagText =
    _Tag . _TagText
  _TagComment ::
    Prism' a str
  _TagComment =
    _Tag . _TagComment
  _TagWarning ::
    Prism' a str
  _TagWarning =
    _Tag . _TagWarning
  _TagPosition ::
    Prism' a (Row, Column)
  _TagPosition =
    _Tag . _TagPosition

instance AsTag (Tag str) str where
  _Tag =
    id

instance AsTag (TagTree str) str where
  _Tag =
    _TagLeaf . _Tag

----

newtype TagTreeW str =
  TagTreeW (TagTree str)
  deriving (Eq, Ord, Show)

instance TagTreeW str ~ t =>
  Rewrapped (TagTreeW str) t

instance Wrapped (TagTreeW str) where
  type Unwrapped (TagTreeW str) =
    TagTree str
  _Wrapped' =
    iso
      (\(TagTreeW x) -> x)
      TagTreeW

instance HasTagTree (TagTreeW str) str where
  tagTree =
    _Wrapped

instance AsTagTree (TagTreeW str) str where
  _TagTree =
    _Wrapped

instance AsTag (TagTreeW str) str where
  _Tag =
    _Wrapped . _Tag
-}

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

data TagTree str =
  TagBranch
    str
    [Attribute str]
    [TagTree str]
  deriving (Eq, Ord, Show)

data Tag str
  = TagOpen str [Attribute str]
  | TagClose str
  | TagText str
  | TagComment str
  | TagWarning str
  | TagPosition Row Column

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


{-

class HasTag a str | a -> str where
  tag ::
    Lens' a (Tag str)

instance HasTag (Tag str) str where
  tag =
    id
-}

----

{-

data TagTree str
  = TagBranch str
              [Attribute str]
              [TagTree str]
  | TagLeaf (Tag str)


type Attribute str = (str, str)

data Tag str
  = TagOpen str [Attribute str]
  | TagClose str
  | TagText str
  | TagComment str
  | TagWarning str
  | TagPosition Row Column

type Row = Int
type Column = Int

-}
