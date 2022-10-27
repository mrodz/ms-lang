func main() {
  var result = factorial(34);
  print(result);
}

obj greeter(message) {
  func greet() {
    print(message);
  }
}

func factorial(number) {
  return factorial_init(1, number);
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



















func print(a) {
  native "LOAD $a";
  native "SYSCALLRM PrintLn";
}

func cmp(a, b) {
  var result = false;
  native "EQU $result, $a, $b";
  return result;
}