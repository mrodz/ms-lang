var number = 5;

func main() {
  var result = factorial(number);
  print(a);  
}

func factorial(a) {
  return factorial_init(1, a);
}

func factorial_init(result, limit) {
  result *= limit;
  limit -= 1;

  if cmp(limit, 0) {
    return result;
  } else {
    return factorial_init(result, limit);
  }
}

func cmp(a, b) {
  var result = false;
  native "EQU $result, $a, $b";
  return result;
}

func print(b) {
  native "LOAD $b";
  native "SYSCALLRM PrintLn";
}
