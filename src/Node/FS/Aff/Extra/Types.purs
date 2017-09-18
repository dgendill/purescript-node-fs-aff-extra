module Node.FS.Aff.Extra.Types (
  class HasSystemPath,
  systemPath,
  filePath,
  From(..),
  To(..),
  Target(..),
  Source(..),
  SystemPath(..)
) where

import Data.Monoid (class Monoid, class Semigroup, append, mempty, (<>))
import Data.Newtype (class Newtype, unwrap)
import Node.Path (FilePath, sep)
import Prelude (class Functor, id, ($))

newtype SystemPath a = SystemPath a
newtype From a = From a
newtype To a = To a
newtype Target a = Target a
newtype Source a = Source a

class HasSystemPath f a where
  systemPath :: (Functor f) => f a -> SystemPath a
  filePath :: (Functor f) => f a -> FilePath

instance hasSystemPathSystemPath :: HasSystemPath SystemPath String where
  systemPath = id
  filePath = unwrap

instance hasSystemPathFrom :: HasSystemPath From String where
  systemPath a = SystemPath $ unwrap a
  filePath = unwrap

instance hasSystemPathTo :: HasSystemPath To String where
  systemPath a = SystemPath $ unwrap a
  filePath = unwrap

instance hasSystemPathSource :: HasSystemPath Source String where
  systemPath a = SystemPath $ unwrap a
  filePath = unwrap

instance hasSystemPathTarget :: HasSystemPath Target String where
  systemPath a = SystemPath $ unwrap a
  filePath = unwrap

derive instance newtypeNodeFSAffExtraSystemPath :: Newtype (SystemPath a) _
derive instance functorNodeFSAffExtraSystemPath :: Functor SystemPath
instance semigroupSystemPath :: Semigroup (SystemPath String) where
  append (SystemPath p1) (SystemPath p2) = SystemPath $ p1 <> sep <> p2

instance monoidSystemPath :: Monoid (SystemPath String) where
  mempty = (SystemPath "")

derive instance newtypeNodeFSAffExtraFrom :: Newtype (From a) _
derive instance functorNodeFSAffExtraFrom :: Functor From

derive instance newtypeNodeFSAffExtraTo :: Newtype (To a) _
derive instance functorNodeFSAffExtraTo :: Functor To

derive instance newtypeNodeFSAffExtraTarget :: Newtype (Target a) _ 
derive instance functorNodeFSAffExtraTarget :: Functor Target

derive instance newtypeNodeFSAffExtraSource :: Newtype (Source a) _
derive instance functorNodeFSAffExtraSource :: Functor Source