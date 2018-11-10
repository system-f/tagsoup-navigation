{-# LANGUAGE NoImplicitPrelude #-}

module Text.HTML.TagSoup.Navigation.Parse.Parse(
  parseTags
, parseTagsOptions
, parseOptions'
, parseOptionsFast'
, parseOptionsEntities
, parseTree
, parseTreeOptions
) where

import Control.Category((.))
import Control.Lens(( # ), (^.))
import Data.Functor(fmap)
import Data.Maybe(Maybe)
import Text.HTML.TagSoup.Navigation.Parse.ParseOptions(ParseOptions, tagsoupParseOptions)
import Text.HTML.TagSoup.Navigation.Types.Tag(Tag, tagsoupTag)
import Text.HTML.TagSoup.Navigation.Types.TagTree(TagTree, tagsoupTagTree)
import qualified Text.HTML.TagSoup as TagSoup(parseTags, parseTagsOptions, parseOptions, parseOptionsFast, parseOptionsEntities)
import qualified Text.HTML.TagSoup.Tree as TagSoup(parseTree, parseTreeOptions)
import Text.StringLike(StringLike)

parseTags ::
  StringLike str =>
  str
  -> [Tag str]
parseTags =
  fmap (tagsoupTag #) . TagSoup.parseTags

parseTagsOptions ::
  StringLike str =>
  ParseOptions str
  -> str
  -> [Tag str]
parseTagsOptions o =
  fmap (tagsoupTag #) . TagSoup.parseTagsOptions (o ^. tagsoupParseOptions)

parseOptions' ::
  StringLike str =>
  ParseOptions str
parseOptions' = 
  tagsoupParseOptions # TagSoup.parseOptions

parseOptionsFast' ::
  StringLike str =>
  ParseOptions str
parseOptionsFast' = 
  tagsoupParseOptions # TagSoup.parseOptionsFast

parseOptionsEntities ::
  StringLike str =>
  (str -> Maybe str)
  -> ParseOptions str
parseOptionsEntities f = 
  tagsoupParseOptions # TagSoup.parseOptionsEntities f

parseTree ::
  StringLike str =>
  str
  -> [TagTree str]
parseTree =
  fmap (tagsoupTagTree #) . TagSoup.parseTree

parseTreeOptions ::
  StringLike str =>
  ParseOptions str
  -> str
  -> [TagTree str] 
parseTreeOptions o =
  fmap (tagsoupTagTree #) . TagSoup.parseTreeOptions (o ^. tagsoupParseOptions)
