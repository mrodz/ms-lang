func main() {
  var b = true || false;
  native "LOAD $b";
  native "SYSCALLRM PrintLn";
}