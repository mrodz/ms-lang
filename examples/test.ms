func main() {
  var b = true and false;
  native "LOAD $b";
  native "SYSCALLRM PrintLn";
}