# sir

An informal Runtime Type Information crate.

```rust
#[derive(Debug)]
struct Example {
    foo: Result<bool, i32>,
    x: bool,
    data: Vec<i32>,
    map: HashMap<u8, bool>,
}
impl sir::Blade for Example {
    sir::blade! {
        struct Example where {},
        foo: Result<bool, i32>,
        x: bool,
        data: Vec<i32>,
        map: HashMap<u8, bool>,
    }
}
```

There are these advantages over serde:
1. Can be used with 3rd-party types: the orphan rule is not a problem.
2. Interaction with different variations on a type (eg, renamed fields, older versions) can be accomodated by creating a new `Blade`.
3. Compile-time cost is `O(number of types)` rather than `O(number of types * number of serde clients)`.
4. Three speeds exposed to knights:
    1. Directly intepreting the RTTI for things rarely used (eg, debugging)
    2. Compiling to an efficient closure-VM at runtime for things that need to be faster.
    3. Using a traditional derive (eg of serde (or manual implementation)) for larger & more complicated types requiring bulk storage, like `Vec<Triangle>`.

Our equivalents of eg `serde_json` are called Knights. The brave knights of this repository are:
1. Sir Vey. Debug printing.
2. Sir Mun. Deserializes rlua objects.
3. Sir Mark. Serializes to lua source code.
4. Sir Eel. Serializes and Deserializes using `crate:bincode`.

(`crates:sir` is already taken; this will have to be renamed if it ever ends up there.)
