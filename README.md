# Type inference for higher rank polymorphism

Doesn't need to be as flexible as other systems:

```haskell
f : (() -> () -> ()) -> ()

g : () -> (forall a. a -> a)

f g -- Is a type error
```

However this needs to work
```haskell
id : forall a. a -> a

f : (() -> ()) -> ()
g : (forall b. b -> b) -> ()

f id -- type application is inferred
g id -- passing polymorphic values is allowed
```

TODO
- [ ] Can forall escape?
