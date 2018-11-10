{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.HTML.TagSoup.Navigation.Types.TagTree(
  TagTree(..)
, HasTagTree(..)
, AsTagTree(..)
) where

import Control.Lens
import Prelude
import Text.HTML.TagSoup.Navigation.Types.Attribute
import Text.HTML.TagSoup.Navigation.Types.Tag

data TagTree str =
  TagBranch
    str
    [Attribute str]
    [TagTree str]
  | TagLeaf (Tag str)
  deriving (Eq, Ord, Show)

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
    