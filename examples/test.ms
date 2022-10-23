func main() {
  var b = fizz();

  native "LOAD $b";
  native "SYSCALLRM PrintLn";
}

func fizz() {
  return true;
}