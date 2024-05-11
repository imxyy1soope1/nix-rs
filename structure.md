```nix
{ a = 1; }
```
|
|
v
```
Code:
  Attrs
  Const 0
  PushStaticAttr 0
Consts:
  1
Symbols:
  a
```

```nix
[ 1 ]
```
|
|
v
```
Code:
  List
  Const 0
  PushElem
Consts:
  1
Symbols:
  
```

```nix
{ a = 1; }.a
```
|
|
v
```
Code:
  Attrs
  Const 0
  PushStaticAttr 0
  Select 0
Consts:
  1
Symbols:
  a
```
