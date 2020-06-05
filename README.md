# Type inference for higher rank polymorphism

...that doesn't support subtyping (on purpose).

```haskell
withMono : (() -> () -> ()) -> ()
higherRank : () -> (forall a. a -> a)

withMono higherRank -- Is a type error
```

However this works
```haskell
id : forall a. a -> a

withMono : (() -> ()) -> ()
withPoly : (forall b. b -> b) -> ()

withMono id -- type application is inferred
withPoly id -- passing polymorphic values is allowed
```

Forall variables are not allowed to escape their scope by the use of an
[ordered context](./src/Simple/Ctx.hs).
