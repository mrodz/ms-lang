func main() {
  var a = factorial(30);
  print(a);
}

func factorial(number) {
  return factorial_i(1, number);
}

func factorial_i(result, number) {
  result *= number;
  number -= 1;
  if equ(number, 0) {
    return result;
  } else {
    return factorial_i(result, number);
  }
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