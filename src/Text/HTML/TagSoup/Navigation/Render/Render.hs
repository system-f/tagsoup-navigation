{-# LANGUAGE NoImplicitPrelude #-}

module Text.HTML.TagSoup.Navigation.Render.Render(
  renderTags
, renderTagsOptions
, renderOptions'
, renderTree
, renderTreeOptions
) where

import Control.Category((.))
import Control.Lens(( # ), (^.))
import Data.Functor(fmap)
import Text.HTML.TagSoup.Navigation.Render.RenderOptions(RenderOptions, tagsoupRenderOptions)
import Text.HTML.TagSoup.Navigation.Types.Tag(Tag, tagsoupTag)
import Text.HTML.TagSoup.Navigation.Types.TagTree(TagTree, tagsoupTagTree)
import qualified Text.HTML.TagSoup as TagSoup(renderTags, renderTagsOptions, renderOptions)
import qualified Text.HTML.TagSoup.Tree as TagSoup(renderTree, renderTreeOptions)
import Text.StringLike(StringLike)

renderTags ::
  StringLike str =>
  [Tag str]
  -> str
renderTags =
  TagSoup.renderTags . fmap (^. tagsoupTag)

renderTagsOptions ::
  StringLike str =>
  RenderOptions str
  -> [Tag str]
  -> str 
renderTagsOptions o =
  TagSoup.renderTagsOptions (o ^. tagsoupRenderOptions) . fmap (^. tagsoupTag)

renderOptions' ::
  StringLike str =>
  RenderOptions str 
renderOptions' =
  tagsoupRenderOptions # TagSoup.renderOptions

renderTree ::
  StringLike str =>
  [TagTree str]
  -> str
renderTree =
  TagSoup.renderTree . fmap (^. tagsoupTagTree)

renderTreeOptions ::
  StringLike str =>
  RenderOptions str
  -> [TagTree str]
  -> str
renderTreeOptions o =
  TagSoup.renderTreeOptions (o ^. tagsoupRenderOptions) . fmap (^. tagsoupTagTree)
