func main() {
  var a = 5 is 5;
  print(a);
}

func print(v) {
  native "LOAD $v";
  native "SYSCALLRM PrintLn";
}