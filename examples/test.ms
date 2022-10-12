var a = true;

func main() {
  var b = a || !!false ^ !true;
  native "LOAD $b";
  native "SYSCALLRM PrintLn";
}