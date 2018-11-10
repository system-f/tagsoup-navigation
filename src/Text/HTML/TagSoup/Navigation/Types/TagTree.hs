{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.HTML.TagSoup.Navigation.Types.TagTree(
  TagTree(..)
, HasTagTree(..)
, AsTagTree(..)
, tagsoupTagTree
, tagTreeBranchNames
, tagTreeChildren
, tagTreeAttributes
, tagTreeAttributeNames
, tagTreeAttributeValues
) where

import Control.Applicative((<*>))
import Control.Category((.), id)
import Control.Lens(Plated(plate), Each(each), Lens', Prism', Iso, Traversal', prism', iso, (^.), ( # ), _1, _2, _3)
import Data.Eq(Eq)
import Data.Eq.Deriving(deriveEq1)
import Data.Foldable(Foldable(foldMap))
import Data.Functor(Functor(fmap), (<$>))
import Data.Ord(Ord)
import Data.Ord.Deriving(deriveOrd1)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Monoid(mappend)
import Data.Traversable(Traversable(traverse))
import Prelude(Show)
import Text.HTML.TagSoup.Navigation.Types.Attribute(Attribute, tagsoupAttribute, attributeName, attributeValue)
import Text.HTML.TagSoup.Navigation.Types.Tag(Tag, AsTag(_Tag), tagsoupTag)
import qualified Text.HTML.TagSoup.Tree as TagSoup(TagTree(TagBranch, TagLeaf))
import Text.Show.Deriving(deriveShow1)

data TagTree str =
  TagBranch
    str
    [Attribute str]
    [TagTree str]
  | TagLeaf (Tag str)
  deriving (Eq, Ord, Show)

instance Functor TagTree where
  fmap f (TagBranch s as ts) =
    TagBranch (f s) (fmap (fmap f) as) (fmap (fmap f) ts)
  fmap f (TagLeaf t) =
    TagLeaf (fmap f t)

instance Foldable TagTree where
  foldMap f (TagBranch s as ts) =
    f s `mappend` foldMap (foldMap f) as `mappend` foldMap (foldMap f) ts
  foldMap f (TagLeaf t) =
    foldMap f t

instance Traversable TagTree where
  traverse f (TagBranch s as ts) =
    TagBranch <$> f s <*> traverse (traverse f) as <*> traverse (traverse f) ts
  traverse f (TagLeaf t) =
    TagLeaf <$> traverse f t

instance Plated (TagTree str) where
  plate =
    tagTreeChildren

instance Each (TagTree str) (TagTree str') str str' where
  each =
    traverse

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

instance AsTag (TagTree str) str where
  _Tag =
    _TagLeaf . _Tag

tagsoupTagTree ::
  Iso (TagTree str) (TagTree str') (TagSoup.TagTree str) (TagSoup.TagTree str')
tagsoupTagTree =
  iso
    (\tr ->
      case tr of
        TagBranch s as t ->
          TagSoup.TagBranch s (fmap (^. tagsoupAttribute) as) (fmap (^. tagsoupTagTree) t)
        TagLeaf x ->
          TagSoup.TagLeaf (x ^. tagsoupTag))
    (\tr ->
      case tr of
        TagSoup.TagBranch s as t ->
          TagBranch s (fmap (tagsoupAttribute #) as) (fmap (tagsoupTagTree #) t)
        TagSoup.TagLeaf x ->
          TagLeaf (tagsoupTag # x))

tagTreeBranchNames ::
  AsTagTree a str =>
  Traversal' a str
tagTreeBranchNames =
  _TagBranch . _1

tagTreeAttributes ::
  AsTagTree a str =>
  Traversal' a (Attribute str)
tagTreeAttributes =
  _TagBranch . _2 . traverse

tagTreeChildren ::
  AsTagTree a str =>
  Traversal' a (TagTree str)
tagTreeChildren =
  _TagBranch . _3 . traverse

tagTreeAttributeNames ::
  AsTagTree a str =>
  Traversal' a str
tagTreeAttributeNames =
  tagTreeAttributes . attributeName

tagTreeAttributeValues ::
  AsTagTree a str =>
  Traversal' a str
tagTreeAttributeValues =
  tagTreeAttributes . attributeValue

deriveEq1 ''TagTree
deriveOrd1 ''TagTree
deriveShow1 ''TagTree
