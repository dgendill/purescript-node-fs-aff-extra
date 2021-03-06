## Module Node.FS.Aff.Extra.Simple

#### `mkdirSafe`

``` purescript
mkdirSafe :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
```

Creates a new folder. Unlike mkdir, safeMkdir will
not throw an error if the folder already exists

#### `rmdirRecurse`

``` purescript
rmdirRecurse :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
```

Removes a folder and all subfolders. Unlike rmdir,
rmdirRecurse will remove folders even if they're
not empty.

#### `overwriteFile`

``` purescript
overwriteFile :: forall eff. FilePath -> Buffer -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
```

Delete an existing file, and then write a new one
using the buffer.

#### `overwriteDir`

``` purescript
overwriteDir :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
```

Delete a directory, and then create a new one by
copying the contents of the source directory

#### `mergeDir`

``` purescript
mergeDir :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
```

Merge the contents of one directory into another.
If a file exists in both source and target, source
is overwritten by target

#### `copyFile`

``` purescript
copyFile :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
```

Copy a file from the source location to the target location.
If target already exists, it will be overwritten.


