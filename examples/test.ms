func main() {
  var d = dog();
  var b = d.a()
  ;
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