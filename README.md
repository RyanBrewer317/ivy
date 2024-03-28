# Ivy

A scripting language written in Gleam! 

Currently a tiny fragment of it works:

```
fn foo(s string, i int) string {
  i;
  s
}

fn main() {
  foo("a", 3);
  println(foo("hello world", 7))
}
```

This parses, typechecks, and executes successfully, being compiled to JavaScript.

Ivy will be an immutable language with no garbage collector, using inferred cloning instead. There will be no lambdas (though code will generally be written in a functional style due to immutability) and thus no closures. In addition, there will be Go-like interfaces but no parametric polymorphism, and there will be nonuniform memory layout like C. In-place updates will be inferred for owned values, which is many of them in this often-cloning language. The pervasive datastructure sharing in functional languages won't be possible here. In the long run, datastructures will be automatically flattened into arrays by the compiler and recursive traversals will be compiled to vectorized loops (see the Gibbon compiler). Lastly, owned values will be convertible into buffers that can be used as the memory to allocate new values of the same type; think of this as "deinitializing" and "reinitializing" a value in-place in memory, without compromising immutability.

These performance and memory tradeoffs are significant in spite of compiling to javascript, as they play to the strengths of V8. However, in the long run, compiling to C, SaberVM, WebAssembly, or LLVM are goals. Uxn might also be a goal, to support that cool project.

Ivy explores a tradeoff for immutable languages. Pervasive sharing requires garbage collection, because things don't own their subcomponents and can't free them. Thus Ivy throws that away with the slogan of "compiling immutable recursion over trees to in-place loops over arrays." 

To accomplish this, it performs a borrow-checking analysis like that of Mojo, borrow-by-default. However, instead of a compile-time error, it inserts clones where needed. For this to work, borrowed values can't be put into datastructures, and are generally copied in. 

To make this more palateable, Ivy's type system allows marking owned values as junk, which aren't readable, and can be passed to a function as the place to allocate its return value (think NRVO). This allows avoiding copies in some cases. Therefore, instead of being truly immutable, Ivy can be seen as a "mutation-is-initialization"-based language: if you can prove that a value is garbage, then you can "initialize" it again with a new value of the same type, cancelling out a free and a malloc.

Ivy is named after my brilliant, beautiful philosopher wife :)