{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Text.HTML.TagSoup.Navigation where


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
