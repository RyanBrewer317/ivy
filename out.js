

class Nat {
  Zero() {
    this.tag = 0;
    ;
    return this;
  }
  Successor(x0) {
    this.tag = 1;
    this.x0 = x0;
    return this;
  }
}


function foo(x0, x1) {
  x1;
  return x0;
}

function main() {
  foo("a", new Nat().Zero());
  return console.log(foo("hello world", new Nat().Successor(new Nat().Zero())));
}

main();