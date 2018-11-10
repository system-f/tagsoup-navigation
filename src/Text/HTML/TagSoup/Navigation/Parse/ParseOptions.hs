{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.HTML.TagSoup.Navigation.Parse.ParseOptions(
  ParseOptions(..)
, HasParseOptions(..)
, AsParseOptions(..)
, tagsoupParseOptions
, boolParseOptions
) where

import Control.Lens(Lens', Prism', Iso, Traversal', ( # ), from, iso, (^.))
import Text.StringLike(StringLike)
import qualified Text.HTML.TagSoup as TagSoup(ParseOptions(ParseOptions), parseOptions)
import Text.HTML.TagSoup.Navigation.Types.Tag(Tag, tagsoupTag)
import Data.Bool(Bool, (&&))
import Control.Category((.), id)
import Data.Functor(Functor(fmap), (<$>))
import Control.Applicative((<*>), pure)
import Data.Semigroup(Semigroup((<>)))
import Data.Monoid(Monoid(mappend, mempty))

data ParseOptions str =
  ParseOptions
    Bool
    Bool
    ((str, Bool) -> [Tag str])
    ((str, Bool) -> (str, [Tag str]))
    Bool

class HasParseOptions s str | s -> str where
  parseOptions ::
    Lens' s (ParseOptions str)
  optTagPosition ::
    Lens' s Bool
  optTagPosition =
    parseOptions . optTagPosition
  optTagWarning ::
    Lens' s Bool
  optTagWarning =
    parseOptions . optTagWarning
  optEntityDate ::
    Lens' s ((str, Bool) -> [Tag str])
  optEntityDate =
    parseOptions . optEntityDate
  optEntityAttrib ::
    Lens' s ((str, Bool) -> (str, [Tag str]))
  optEntityAttrib =
    parseOptions . optEntityAttrib
  optTagTextMerge ::
    Lens' s Bool
  optTagTextMerge =
    parseOptions . optTagTextMerge

instance HasParseOptions (ParseOptions str) str where
  parseOptions =
    id
  optTagPosition f (ParseOptions p w ed ea tm) =
    fmap (\p' -> ParseOptions p' w ed ea tm) (f p)
  optTagWarning f (ParseOptions p w ed ea tm) =
    fmap (\w' -> ParseOptions p w' ed ea tm) (f w)
  optEntityDate f (ParseOptions p w ed ea tm) =
    fmap (\ed' -> ParseOptions p w ed' ea tm) (f ed)
  optEntityAttrib f (ParseOptions p w ed ea tm) =
    fmap (\ea' -> ParseOptions p w ed ea' tm) (f ea)
  optTagTextMerge f (ParseOptions p w ed ea tm) =
    fmap (\tm' -> ParseOptions p w ed ea tm') (f tm)

class AsParseOptions s str | s -> str where
  _ParseOptions ::
    Prism' s (ParseOptions str)

instance AsParseOptions (ParseOptions str) str where
  _ParseOptions =
    id

instance Semigroup str => Semigroup (ParseOptions str) where
  ParseOptions p1 w1 ed1 ea1 tm1 <> ParseOptions p2 w2 ed2 ea2 tm2 =
    ParseOptions (p1 && p2) (w1 && w2) (\z -> ed1 z <> ed2 z) (\z -> ea1 z <> ea2 z) (tm1 && tm2)

instance (Monoid str, StringLike str) => Monoid (ParseOptions str) where
  mempty =
    tagsoupParseOptions # TagSoup.parseOptions
  ParseOptions p1 w1 ed1 ea1 tm1 `mappend` ParseOptions p2 w2 ed2 ea2 tm2 =
    ParseOptions (p1 && p2) (w1 && w2) (\z -> ed1 z `mappend` ed2 z) (\z -> ea1 z `mappend` ea2 z) (tm1 && tm2)

instance HasParseOptions (TagSoup.ParseOptions str) str where
  parseOptions =
    from tagsoupParseOptions . parseOptions

instance AsParseOptions (TagSoup.ParseOptions str) str where
  _ParseOptions =
    from tagsoupParseOptions . _ParseOptions

tagsoupParseOptions ::
  Iso (ParseOptions str) (ParseOptions str') (TagSoup.ParseOptions str) (TagSoup.ParseOptions str')
tagsoupParseOptions =
  iso
    (\(ParseOptions p w ed ea tm) ->
      TagSoup.ParseOptions p w (fmap (^. tagsoupTag) . ed) (fmap (fmap (^. tagsoupTag)) . ea) tm)
    (\(TagSoup.ParseOptions p w ed ea tm) ->
      ParseOptions p w (fmap (tagsoupTag #) . ed) (fmap (fmap (tagsoupTag #)) . ea) tm)

boolParseOptions ::
  Traversal' (ParseOptions str) Bool
boolParseOptions f (ParseOptions p w ed ea tm) =
  ParseOptions <$> f p <*> f w <*> pure ed <*> pure ea <*> f tm
