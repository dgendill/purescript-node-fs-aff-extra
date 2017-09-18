module Node.FS.Aff.Extra.Simple where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Traversable (traverse)
import Node.Buffer (Buffer, BUFFER)
import Node.FS (FS)
import Node.FS.Aff (exists, mkdir, readFile, readdir, rmdir, stat, unlink, writeFile)
import Node.FS.Stats (isDirectory)
import Node.Path (FilePath, concat, sep)

-- | Creates a new folder. Unlike mkdir, safeMkdir will
-- | not throw an error if the folder already exists
mkdirSafe :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
mkdirSafe f = exists f >>= (\doesExist -> if (not doesExist) then mkdir f else pure unit)

-- | Removes a folder and all subfolders. Unlike rmdir,
-- | rmdirRecurse will remove folders even if they're
-- | not empty.
rmdirRecurse :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
rmdirRecurse d = do 
  exist <- exists d
  if (not exist)
    then pure unit
    else do
      filesToDelete <- readdir d
      void $ traverse (\f -> do
        let path = concat [d, f]
        s <- stat path
        if isDirectory s then rmdirRecurse path else unlink path ) filesToDelete
      rmdir d

  
-- | Delete an existing file, and then write a new one
-- | using the buffer.
overwriteFile :: forall eff
   . FilePath
  -> Buffer
  -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
overwriteFile f b = do
  exist <- exists f
  if (exist) then unlink f
             else pure unit
  writeFile f b 

-- | Delete a directory, and then create a new one by
-- | copying the contents of the source directory
overwriteDir :: forall eff
   . FilePath
  -> FilePath
  -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
overwriteDir target source = do
  rmdirRecurse target
  mergeDir source target

-- | Merge the contents of one directory into another.
-- | If a file exists in both source and target, source
-- | is overwritten by target
mergeDir :: forall eff
   . FilePath
  -> FilePath
  -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
mergeDir from to = do
  filesToCopy <- readdir from
  mkdirSafe to
  void $ traverse (\f -> do
    let from' = from <> sep <> f
    let to' = to <> sep <> f
    copyFile from' to'
  ) filesToCopy
  pure unit


-- | Copy a file from the source location to the target location.
-- | If target already exists, it will be overwritten.
copyFile  :: forall eff
   . FilePath
  -> FilePath
  -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
copyFile from to = do
  stats <- stat from
  case isDirectory stats of
    true -> pure unit
    false -> do
      b <- readFile from
      writeFile to b