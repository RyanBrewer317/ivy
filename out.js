

function foo(x0, x1) {
  x1;
  return x0;
}

function main() {
  foo("a", 3);
  return console.log(foo("hello world", 7));
}

main();