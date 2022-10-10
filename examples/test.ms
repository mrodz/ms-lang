var pi = 3.141592;
var xy = [pi, true, false];

func main() {
  var a = [1, 2, pi, xy, [["a", "b"], "c"]];

  native "LOAD $a";
  native "SYSCALLRM PrintLn";
}