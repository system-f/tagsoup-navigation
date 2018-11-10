{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.HTML.TagSoup.Navigation.Types.Tag(
  Tag(..)
, HasTag(..)
, AsTag(..)
, tagstr
, tagAttributes
, tagAttributeNames
, tagAttributeValues
, tagRows
, tagColumns
) where

import Control.Applicative((<*>), pure)
import Control.Category(id, (.))
import Control.Lens(Lens', Prism', Traversal, Traversal', Each(each), _1, _2)
import Data.Eq(Eq)
import Data.Eq.Deriving(deriveEq1)
import Data.Ord(Ord)
import Data.Ord.Deriving(deriveOrd1)
import Data.Foldable(Foldable(foldMap))
import Data.Functor(Functor(fmap), (<$>))
import Data.Monoid(mappend, mempty)
import Data.Semigroup(Semigroup((<>)))
import Data.Traversable(Traversable(traverse))
import Prelude(Show, (+))
import Text.HTML.TagSoup.Navigation.Types.Attribute(Attribute, Row, Column, attributeName, attributeValue)
import Text.Show.Deriving(deriveShow1)

data Tag str
  = TagOpen str [Attribute str]
  | TagClose str
  | TagText str
  | TagComment str
  | TagWarning str
  | TagPosition Row Column
  deriving (Eq, Ord, Show)

instance Functor Tag where
  fmap f (TagOpen s as) =
    TagOpen (f s) (fmap (fmap f) as)
  fmap f (TagClose s) =
    TagClose (f s)
  fmap f (TagText s) =
    TagText (f s)
  fmap f (TagComment s) =
    TagComment (f s)
  fmap f (TagWarning s) =
    TagWarning (f s)
  fmap _ (TagPosition r c) =
    TagPosition r c

instance Foldable Tag where
  foldMap f (TagOpen s as) =
    f s `mappend` foldMap (foldMap f) as
  foldMap f (TagClose s) =
    f s
  foldMap f (TagText s) =
    f s
  foldMap f (TagComment s) =
    f s
  foldMap f (TagWarning s) =
    f s
  foldMap _ (TagPosition _ _) =
    mempty

instance Traversable Tag where
  traverse f (TagOpen s as) =
    TagOpen <$> f s <*> traverse (traverse f) as
  traverse f (TagClose s) =
    TagClose <$> f s
  traverse f (TagText s) =
    TagText <$> f s
  traverse f (TagComment s) =
    TagComment <$> f s
  traverse f (TagWarning s) =
    TagWarning <$> f s
  traverse _ (TagPosition r c) =
    pure (TagPosition r c)

instance Semigroup str => Semigroup (Tag str) where
  TagOpen s1 as1 <> TagOpen s2 as2 =
    TagOpen (s1 <> s2) (as1 <> as2)
  TagClose s1 <> TagClose s2 =
    TagClose (s1 <> s2)
  TagText s1 <> TagText s2 =
    TagText (s1 <> s2)
  TagComment s1 <> TagComment s2 =
    TagComment (s1 <> s2)
  TagWarning s1 <> TagWarning s2 =
    TagWarning (s1 <> s2)
  TagPosition r1 c1 <> TagPosition r2 c2 =
    TagPosition (r1 + r2) (c1 + c2)
  x <> _ =
    x

deriveEq1 ''Tag
deriveOrd1 ''Tag
deriveShow1 ''Tag

instance Each (Tag str) (Tag str) str str where
  each =
    tagstr

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

tagstr ::
  Traversal (Tag str) (Tag str') str str'
tagstr f (TagOpen a as) =
  TagOpen <$> f a <*> traverse (traverse f) as
tagstr f (TagClose s) =
  TagClose <$> f s
tagstr f (TagText s) =
  TagClose <$> f s
tagstr f (TagComment s) =
  TagClose <$> f s
tagstr f (TagWarning s) =
  TagClose <$> f s
tagstr _ (TagPosition r c) =
  pure (TagPosition r c)

tagAttributes ::
  AsTag a str =>
  Traversal' a (Attribute str)
tagAttributes =
  _TagOpen . _2 . traverse

tagAttributeNames ::
  AsTag a str =>
  Traversal' a str
tagAttributeNames =
  tagAttributes . attributeName

tagAttributeValues ::
  AsTag a str =>
  Traversal' a str
tagAttributeValues =
  tagAttributes . attributeValue

tagRows ::
  AsTag a str =>
  Traversal' a Row
tagRows =
  _TagPosition . _1
  
tagColumns ::
  AsTag a str =>
  Traversal' a Column
tagColumns =
  _TagPosition . _2
