func main() {
  var b = not(not(neq([1/3, 2], [1/3, 2])));
  print(b);
}

func equ(a, b) {
  var result = false;
  native "EQU $result, $a, $b";
  return result;
}

func not(a) {
  native "NOT $a";
  return a;
}

func neq(a, b) {
  var result = false;
  native "NEQ $result, $a, $b";
  return result;
}

func and(a, b) {
  native "AND $a, $b";
  return a;
}

func print(v) {
  native "LOAD $v";
  native "SYSCALLRM PrintLn";
}