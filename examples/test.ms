var hello = "Hello World!";
var a = -13;

func main() {
  var b = 2;
  var c = (-(a * b));
  native "LOAD $c";
  native "SYSCALLRM PrintLn";
}