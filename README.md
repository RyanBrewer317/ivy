# Ivy

<!-- 
```sh
gleam add ivy
```
```gleam
import ivy

pub fn main() {
  // TODO: An example of the project in use
}
```

Further documentation can be found at <https://hexdocs.pm/ivy>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
gleam shell # Run an Erlang shell
``` -->

A scripting language written in Gleam! 

Currently a tiny fragment of it can be parsed and typechecked:

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

This parses and passes the typechecker. It will soon compile to javascript.

Ivy will be an immutable language with no garbage collector, using inferred cloning instead. There will be no lambdas (though code will generally be written in a functional style due to immutability) and thus no closures. In addition, there will be Go-like interfaces but no parametric polymorphism, and there will be nonuniform memory layout like C. In-place updates will be inferred for owned values, which is many of them in this often-cloning language. The pervasive datastructure sharing in functional languages won't be possible here. In the long run, datastructures will be automatically flattened into arrays by the compiler and recursive traversals will be compiled to vectorized loops (see the Gibbon compiler). Lastly, owned values will be convertible into buffers that can be used as the memory to allocate new values of the same type; think of this as "deinitializing" and "reinitializing" a value in-place in memory, without compromising immutability.

These performance and memory tradeoffs are significant in spite of compiling to javascript, as they play to the strengths of V8. However, in the long run, compiling to C, SaberVM, WebAssembly, or LLVM are goals. Uxn might also be a goal, to support that cool project.