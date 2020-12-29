---
title: Deal with Default Values of row-types in Haskell
date: 2020-12-29
category: [ haskell, row-types ]
---

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}
```

Start with an interface in TypeScript:

```typescript
interface Type {
    a: string;
    b: string;
}
```

Let's say we want to describe this interface in Haskell, and also a function corresponding to the following function in TypeScript:

```typescript
let defaultValue: Type = ...;

function initialize(params: Partial<Type>) {
    return {
        ...defaultValue,
        ...params,
    };
}
```

We can implement it using `row-types` library, as follows:

```haskell
import Control.Lens ((&))
import Data.Generics.Labels ()
import Data.Row (type (.!), type (.+), type (.==), Rec, Var, (.!), (.+), (.==))
import Data.Row.Records (Label(Label))
import qualified Data.Row.Variants as Var

type TypeRow = "a" .== String .+ "b" .== String

type Type = Rec TypeRow

-- Partial<Type>
type PartialType = Var TypeRow

defaultValue :: Type
defaultValue = #a .== "default for a" .+ #b .== "default for b"

initialize :: PartialType -> Type
initialize v = #a .== query #a .+ #b .== query #b
  where
    query :: Var.KnownSymbol l => Label l -> TypeRow .! l
    query l = case Var.trial v l of
        Left a -> a
        _ -> defaultValue .! l

partialValue :: PartialType
partialValue = Var.IsJust #a "value for a"

fullValue :: Type
fullValue = initialize partialValue
```
