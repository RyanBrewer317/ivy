

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


function plus(x0, x1) {
  ;
  return (()=>{switch(x0.tag) {
    case 0: {

    ;
    return x1;
  }
    case 1: {
let x2 = x0.x0;
    ;
    return new Nat().Successor(plus(x2, x1));
  }
  }})();
}

function times(x3, x4) {
  ;
  return (()=>{switch(x3.tag) {
    case 0: {

    ;
    return new Nat().Zero();
  }
    case 1: {
let x5 = x3.x0;
    ;
    return plus(times(x5, x4), x4);
  }
  }})();
}

function nat_to_dots(x6) {
  ;
  return (()=>{switch(x6.tag) {
    case 0: {

    ;
    return console.log("");
  }
    case 1: {
let x7 = x6.x0;
    console.log(".");
    return nat_to_dots(x7);
  }
  }})();
}

function main() {
  ;
  return nat_to_dots(times(new Nat().Successor(new Nat().Successor(new Nat().Successor(new Nat().Zero()))), new Nat().Successor(new Nat().Successor(new Nat().Zero()))));
}

main();