var a = true;

func main() {
  var b = !a || false;
  native "LOAD $b";
  native "SYSCALLRM PrintLn";
}