func fizz(num, bool) {
  native "LOAD $num, $bool";
  native "SYSCALLRM PrintLn";
}

func main() {
  fizz((5 + -10 / 2 * (3 - 1)), true || false);
}