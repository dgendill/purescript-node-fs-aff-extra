# purescript-node-fs-aff-extra

[![Build Status](https://travis-ci.org/dgendill/purescript-node-fs-aff-extra.svg?branch=master)](https://travis-ci.org/dgendill/purescript-node-fs-aff-extra)

Provides functions not included in [purescript-node-fs-aff](https://github.com/purescript-node/purescript-node-fs-aff), such as `copyFile`, `mergeDir`, `overwriteDir`, `overwriteFile`, and `rmdirRecursive`. [Node.FS.Aff.Extra](./generated-docs/Node/FS/Aff/Extra.md) uses newtype wrappers around FilePaths, so instead of passing file paths like this...

```purescript
copyFile "file" "file2"
```

We can do this...

```
copyFile (From "file") (To "file2")

```

If you want to pass file paths as strings, you can use [Node.FS.Aff.Extra.Simple](./generated-docs/Node/FS/Aff/Extra/Simple.md).

## Development

Pull requests and name changes are welcome. See the `test` folder for tests.  To test run `pulp test`.