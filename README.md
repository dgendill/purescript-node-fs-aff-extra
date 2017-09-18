# purescript-node-fs-aff-extra

Provides functions not included in [purescript-node-fs-aff](https://github.com/purescript-node/purescript-node-fs-aff), such as `copyFile`, `mergeDir`, `overwriteDir`, `overwriteFile`, and `rmdirRecursive`.

[FS.Aff.Extra](./generated-docs/Node/FS/Aff/Extra.md) uses newtype wrappers around FilePaths, so instead of passing filepaths as strings like this...

```purescript
copyFile "file" "file2"
```

We can do this...

```
copyFile (From "file") (To "file2")
```

If you want to pass filepaths as Strings, you can use [Node.FS.Aff.Extra.Simple](./generated-docs/Node/FS/Aff/Extra/Simple.md) which contains the same functions but does not require the newtype wrappers.

# Development

Pull requests and name changes are welcome. See the `test` folder for tests.  Run `pulp test` to run the tests.