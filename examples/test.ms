func main() {
  var d = 5 * 10; 
  print(d);
}

func print(a) {
  native "LOAD $a";
  native "SYSCALLRM PrintLn";
}