{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Text.HTML.TagSoup.Navigation where

import Prelude
import Text.HTML.TagSoup.Navigation.Attribute

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



{-

class HasTag a str | a -> str where
  tag ::
    Lens' a (Tag str)

instance HasTag (Tag str) str where
  tag =
    id
-}


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
