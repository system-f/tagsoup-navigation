{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.HTML.TagSoup.Navigation.Zipper.Zipper(
  TagTreeZipper(..)
) where

-- import Text.HTML.TagSoup.Navigation.Types.Attribute(Attribute)
import Text.HTML.TagSoup.Navigation.Zipper.Parent
import Text.HTML.TagSoup.Navigation.Types.TagTree(TagTree)

data TagTreeZipper str =
  TagTreeZipper
    (TagTree str)
    [TagTree str]
    [TagTree str]
    [Parent str]
