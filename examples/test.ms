func main() {
  var c = 0;

  while true {
    print(c);
    c += 1;
  }
}

func lt(a, b) {
  var result = false;
  native "LT $result, $a, $b";
  return result;
}

func equ(a, b) {
  var result = false;
  native "EQU $result, $a, $b";
  return result;
}

func print(a) {
  native "LOAD $a";
  native "SYSCALLRM PrintLn";
}