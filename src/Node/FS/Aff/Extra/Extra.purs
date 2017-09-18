-- | This module defines some additional functions not included in Node.FS.Aff
-- | and uses newtype wrappers from `Node.FS.Aff.Extra.Types` to wrap FilePath
-- | arguments, e.g. instead of `copyFile "file" "file2"` we use `copyFile (From "file") (To "file2")`.
-- | See `Node.FS.Aff.Extra.Simple` for functions don't require the newtype wrappers.
module Node.FS.Aff.Extra (
  mkdirSafe,
  rmdirRecurse,
  overwriteFile,
  overwriteDir,
  mergeDir,
  copyFile,
  writeTextFile
) where
  
import Prelude

import Control.Monad.Aff (Aff)
import Data.Newtype (unwrap)
import Node.Buffer (Buffer, BUFFER)
import Node.Encoding (Encoding)
import Node.FS (FS)
import Node.FS.Aff as FSAff
import Node.FS.Aff.Extra.Simple as S
import Node.FS.Aff.Extra.Types (From(From), Source, SystemPath(SystemPath), Target, To(To))
import Node.Path (FilePath)

writeTextFile :: forall eff
   . Encoding
  -> SystemPath FilePath
  -> String
  -> Aff (fs :: FS | eff) Unit
writeTextFile e (SystemPath p) c = FSAff.writeTextFile e p c

-- | Creates a new folder. Unlike mkdir, safeMkdir will
-- | not throw an error if the folder already exists
mkdirSafe :: forall eff
   . SystemPath FilePath
  -> Aff (fs :: FS | eff) Unit
mkdirSafe (SystemPath f) =
  S.mkdirSafe f

-- | Removes a folder and all subfolders. Unlike rmdir,
-- | rmdirRecurse will remove folders even if they're not empty.
rmdirRecurse :: forall eff
   . SystemPath FilePath
  -> Aff (fs :: FS | eff) Unit
rmdirRecurse (SystemPath d) =
  S.rmdirRecurse d

-- | Delete an existing file, and then write a new one
-- | using the buffer.
overwriteFile :: forall eff
   . SystemPath FilePath
  -> Buffer
  -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
overwriteFile (SystemPath path) buffer =
  S.overwriteFile path buffer

-- | Delete a directory, and then create a new one by
-- | copying the contents of the source directory
overwriteDir :: forall eff
   . Target FilePath
  -> Source FilePath
  -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
overwriteDir target source =
  S.overwriteDir (unwrap target) (unwrap source)

-- | Merge the contents of one directory into another. If a file
-- | exists in both source and target, source is overwritten by target
mergeDir :: forall eff
   . From FilePath
  -> To FilePath
  -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
mergeDir (From from) (To to) =
  S.mergeDir from to

-- | Copy a file from the source location to the target location.
-- | If target already exists, it will be overwritten.
copyFile  :: forall eff
   . From FilePath
  -> To FilePath
  -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
copyFile (From from) (To to) =
  S.copyFile from to