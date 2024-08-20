# Syntax draft
## Custom types
```scala
type Flavor {
    Orange
    , Lemon
}
```

---

## Functions
**Argument names are decoupled from the signature.**
```scala
// Signature declaration, with or without keyword?
my_func : Flavor -> Option(Flavor) -> Bool;

// Exhaustive pattern matching
my_func _ None = True;
my_func flavor Some(fav) = fav == flavor;
```

Also bigger functions:
```scala
// Without equals?
my_func flavor fav {
    match fav {
        None -> True
        Some(fav) -> fav == flavor
    }
}

// With equals?
my_func flavor fav = {
    // ..
}
```

## Function calling
```scala
// Crystal/Haskell like
my_func Orange Some(Orange); // True
p_func = (my_func Orange); // Partial application
p_func Some(Lemon); // False

// C-Like
my_func(Orange, Some(Orange)); // True
```

# Functions as first-class citizens
```scala
map : (U8 -> U8) -> List(U8) -> List(U8);
map f [] = [];
map f (x:xs) = f x : map f xs;
```

# Samples
## Sample 1 - Haskell/ML-Style
```scala
type Flavor {
    Orange
    , Lemon
}

my_func: Flavor -> Option(Flavor) -> Bool;
my_func flavor fav {
    match fav {
        None -> True
        Some(fav) -> fav == flavor
    }
}

main {
    my_fav: Option(Flavor) = Some(Orange);
    res = my_func Orange my_fav;
}
```
