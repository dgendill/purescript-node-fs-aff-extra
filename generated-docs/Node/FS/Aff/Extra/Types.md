## Module Node.FS.Aff.Extra.Types

#### `SystemPath`

``` purescript
newtype SystemPath a
  = SystemPath a
```

##### Instances
``` purescript
HasSystemPath SystemPath String
Newtype (SystemPath a) _
Functor SystemPath
Semigroup (SystemPath String)
Monoid (SystemPath String)
```

#### `From`

``` purescript
newtype From a
  = From a
```

##### Instances
``` purescript
HasSystemPath From String
Newtype (From a) _
Functor From
```

#### `To`

``` purescript
newtype To a
  = To a
```

##### Instances
``` purescript
HasSystemPath To String
Newtype (To a) _
Functor To
```

#### `Target`

``` purescript
newtype Target a
  = Target a
```

##### Instances
``` purescript
HasSystemPath Target String
Newtype (Target a) _
Functor Target
```

#### `Source`

``` purescript
newtype Source a
  = Source a
```

##### Instances
``` purescript
HasSystemPath Source String
Newtype (Source a) _
Functor Source
```

#### `HasSystemPath`

``` purescript
class HasSystemPath f a  where
  systemPath :: Functor f => f a -> SystemPath a
  filePath :: Functor f => f a -> FilePath
```

##### Instances
``` purescript
HasSystemPath SystemPath String
HasSystemPath From String
HasSystemPath To String
HasSystemPath Source String
HasSystemPath Target String
```


