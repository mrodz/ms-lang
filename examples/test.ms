func main() {
  var d = "Hello"; 
  print(d[5]);
}

func print(a) {
  native "LOAD $a";
  native "SYSCALLRM PrintLn";
}