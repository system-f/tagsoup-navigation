{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.HTML.TagSoup.Navigation.Render.RenderOptions(
  RenderOptions(..)
, HasRenderOptions(..)
, AsRenderOptions(..)
, tagsoupRenderOptions
, boolRenderOptions
, xmapRenderOptions
) where

import Text.StringLike(StringLike)
import Control.Lens(Lens', Prism', Iso, Traversal', ( # ), from, iso)
import Data.Bool(Bool, (&&))
import Control.Category((.), id)
import Data.Functor(fmap, (<$>))
import Control.Applicative((<*>))
import Data.Semigroup(Semigroup((<>)))
import Data.Monoid(Monoid(mappend, mempty))
import qualified Text.HTML.TagSoup as TagSoup(RenderOptions(RenderOptions), renderOptions)

data RenderOptions str =
  RenderOptions
    (str -> str)
    (str -> Bool)
    (str -> Bool)

class HasRenderOptions s str | s -> str where
  renderOptions ::
    Lens' s (RenderOptions str)
  optEscape ::
    Lens' s (str -> str)
  optEscape =
    renderOptions . optEscape
  optMinimize ::
    Lens' s (str -> Bool)
  optMinimize =
    renderOptions . optMinimize
  optRawTag ::
    Lens' s (str -> Bool)
  optRawTag =
    renderOptions . optRawTag

instance HasRenderOptions (RenderOptions str) str where
  renderOptions =
    id
  optEscape f (RenderOptions e m r) =
    fmap (\e' -> RenderOptions e' m r) (f e)
  optMinimize f (RenderOptions e m r) =
    fmap (\m' -> RenderOptions e m' r) (f m)
  optRawTag f (RenderOptions e m r) =
    fmap (\r' -> RenderOptions e m r') (f r)

class AsRenderOptions s str | s -> str where
  _RenderOptions ::
    Prism' s (RenderOptions str)

instance AsRenderOptions (RenderOptions str) str where
  _RenderOptions =
    id

instance Semigroup (RenderOptions str) where
  RenderOptions e1 m1 r1 <> RenderOptions e2 m2 r2 =
    RenderOptions (e1 . e2) (\s -> m1 s && m2 s) (\s -> r1 s && r2 s)

instance StringLike str => Monoid (RenderOptions str) where
  mempty =
    tagsoupRenderOptions # TagSoup.renderOptions
  mappend =
    (<>)

instance HasRenderOptions (TagSoup.RenderOptions str) str where
  renderOptions =
    from tagsoupRenderOptions . renderOptions

instance AsRenderOptions (TagSoup.RenderOptions str) str where
  _RenderOptions =
    from tagsoupRenderOptions . _RenderOptions

tagsoupRenderOptions ::
  Iso (RenderOptions str) (RenderOptions str') (TagSoup.RenderOptions str) (TagSoup.RenderOptions str')
tagsoupRenderOptions =
  iso
    (\(RenderOptions e m r) ->
      TagSoup.RenderOptions e m r)
    (\(TagSoup.RenderOptions e m r) ->
      RenderOptions e m r)

boolRenderOptions ::
  Traversal' (RenderOptions str) (str -> Bool)
boolRenderOptions f (RenderOptions e m r) =
  RenderOptions e <$> f m <*> f r

xmapRenderOptions ::
  (str -> str')
  -> (str' -> str)
  -> RenderOptions str
  -> RenderOptions str'
xmapRenderOptions f g (RenderOptions e m r) =
  RenderOptions (f . e . g) (m . g) (r . g)
