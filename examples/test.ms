func main() {
  var d = dog();
  d.bark();
  print(d);
}

func print(a) {
  native "LOAD $a";
  native "SYSCALLRM PrintLn";
}

obj dog() {
  var a = 20;
  func bark() {

  }
}