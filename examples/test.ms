func main() {
  test(a);
}

func test(number) {
  var result = "";

  if equ(number % 5, 0) {
    result = result + "Fizz";
  }

  if equ(number % 3, 0) {
    result = result + "Buzz";
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